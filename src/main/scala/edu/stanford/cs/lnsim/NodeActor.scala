package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.graph.Channel
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.routing.{NetworkGraphView, RouteConstraints, Router}

import scala.collection.mutable
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

class NodeActor(val id: NodeID,
                val params: NodeActor.Params,
                private val router: Router,
                private val graphView: NetworkGraphView,
                private val blockchain: BlockchainView) extends StructuredLogging {
  import NodeActor._

  private val channels: mutable.Map[ChannelID, ChannelView] = mutable.HashMap.empty
  private val pendingPayments: mutable.Map[PaymentID, PendingPayment] = mutable.Map.empty

  def meanNetworkLatency: TimeDelta = 1000

  def channelOpen(channelID: ChannelID,
                  otherNode: NodeID,
                  requiredConfirmations: BlockDelta,
                  localBalance: Value,
                  remoteBalance: Value,
                  localParams: ChannelParams,
                  remoteParams: ChannelParams): Unit = {
    if (channels.contains(channelID)) {
      throw new AssertionError(s"Channel ${channelID} has already been added to node $id")
    }

    logger.info(
      "msg" -> "Opening new channel".toJson,
      "channelID" -> channelID.toJson,
      "thisNode" -> id.toJson,
      "otherNode" -> otherNode.toJson,
      "capacity" -> (localBalance + remoteBalance).toJson
    )
    channels(channelID) = new ChannelView(otherNode, localBalance, remoteBalance, localParams, remoteParams)
    blockchain.subscribeChannelConfirmed(channelID, requiredConfirmations)
  }

  def handleChannelOpenedOnChain(channelID: ChannelID)
                                (implicit actions: NodeActions): Unit = {
    channels.get(channelID) match {
      case Some(channelView) =>
        channelView.transition(ChannelView.Status.Active)
        val channel = channelUpdate(
          channelID = channelID,
          otherNodeID = channelView.otherNode,
          actions.timestamp,
          disabled = false,
          capacity = channelView.ourInitialBalance + channelView.theirInitialBalance,
          theirChannelParams = channelView.theirParams
        )
        graphView.updateChannel(channel)

      case None =>
        throw new AssertionError(s"Cannot handle ChannelOpened for unknown channel $channelID")
    }
  }

  def handleUpdateAddHTLC(sender: NodeID, message: UpdateAddHTLC)
                         (implicit actions: NodeActions): Unit = {
    val UpdateAddHTLC(route) = message
    val (hop :: nextHops) = route.hops

    val channel = channels.getOrElse(
      hop.channel.id,
      throw new HTLCUpdateFailure(s"Node ${id} cannot receive HTLC on unknown channel ${hop.channel.id}")
    )

    // The simulation environment makes the assumption that HTLC updates are atomic for the sake of simplicity,
    // so if an HTLC was added without error on the other end of the channel, there should be no error here either.
    channel.addRemoteHTLC(hop.desc) match {
      case Some(error) =>
        throw new HTLCUpdateFailure(s"Error receiving HTLC ${hop.id} on channel ${hop.channel.id}: ${error}")
      case None =>
    }

    val backwardsRoute = BackwardRoutingPacket((hop.channel, hop.id) :: route.backwardRoute.hops)
    nextHops match {
      case nextHop :: restHops =>
        // Intermediate hop in the circuit.
        val event = sendHTLC(nextHop) match {
          case Left(error) =>
            channel.failRemoteHTLC(hop.id).left.foreach(error => throw new HTLCUpdateFailure(
              s"Error failing newly added HTLC ${hop.id} on channel ${hop.channel.id}: $error"
            ))
            val failMsg = UpdateFailHTLC(backwardsRoute, error, Some(nextHop.channel))
            actions.sendMessage(HTLCUpdateProcessingTime, sender, failMsg)

          case Right(nextHtlc) =>
            logger.debug(
              "msg" -> "Forwarding HTLC".toJson,
              "htlcID" -> nextHtlc.id.toJson,
              "channelID" -> nextHop.channelID.toJson,
              "paymentID" -> nextHtlc.paymentID.toJson,
            )
            val newRoute = ForwardRoutingPacket(nextHtlc :: restHops, route.finalHop, backwardsRoute)
            actions.sendMessage(HTLCUpdateProcessingTime, nextHop.recipient, UpdateAddHTLC(newRoute))
        }

      case Nil =>
        // Final hop in the circuit.
        val delay = HTLCUpdateProcessingTime
        acceptHTLC(hop, route.finalHop) match {
          case Some(error) =>
            channel.failRemoteHTLC(hop.id).left.foreach(error => throw new HTLCUpdateFailure(
              s"Error failing newly added HTLC ${hop.id} on channel ${hop.channel.id}: $error"
            ))
            val failMsg = UpdateFailHTLC(backwardsRoute, error, None)
            actions.sendMessage(delay, sender, failMsg)

          case None =>
            channel.fulfillRemoteHTLC(hop.id).left.foreach(error => throw new HTLCUpdateFailure(
              s"Error fulfilling newly added HTLC ${hop.id} on channel ${hop.channel.id}: $error"
            ))
            actions.sendMessage(delay, sender, UpdateFulfillHTLC(backwardsRoute))
        }
    }
  }

  def handleUpdateFailHTLC(sender: NodeID, failMsg: UpdateFailHTLC)
                          (implicit actions: NodeActions): Unit = {
    val UpdateFailHTLC(route, _, _) = failMsg
    val ((channelInfo, htlcID) :: nextHops) = route.hops
    val channelID = channelInfo.id

    val channel = channels.getOrElse(channelID,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel ${channelID}")
    )

    val htlc = channel.failLocalHTLC(htlcID) match {
      case Right(htlc) => htlc
      case Left(error) => throw new HTLCUpdateFailure(
        s"Error failing HTLC ${htlcID} on channel ${channelID}: $error"
      )
    }

    nextHops match {
      case (nextChannelInfo, nextHtlcID) :: _ =>
        // Intermediate hop in the circuit.
        val nextChannelID = nextChannelInfo.id
        val maybeError = channels.get(nextChannelID) match {
          case Some(nextChannel) => nextChannel.failRemoteHTLC(nextHtlcID).left.toOption
          case None => Some(UnknownNextPeer)
        }
        maybeError.foreach(error => throw new HTLCUpdateFailure(
          s"Error forwarding HTLC ${nextHtlcID} fail on channel ${nextChannelID}: $error"
        ))

        val delay = HTLCUpdateProcessingTime
        val newRoute = BackwardRoutingPacket(nextHops)
        actions.sendMessage(delay, nextChannelInfo.source, failMsg.copy(route = newRoute))
      case Nil =>
        // Error made it back to the original sender.
        failPayment(htlc.paymentID, failMsg.error, failMsg.channel)
    }
  }

  def handleUpdateFulfillHTLC(sender: NodeID, fulfillMsg: UpdateFulfillHTLC)
                             (implicit actions: NodeActions): Unit = {
    val UpdateFulfillHTLC(route) = fulfillMsg
    val ((channelInfo, htlcID) :: nextHops) = route.hops
    val channelID = channelInfo.id

    val channel = channels.getOrElse(channelID,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel ${channelID}")
    )

    val htlc = channel.fulfillLocalHTLC(htlcID) match {
      case Right(htlc) => htlc
      case Left(error) =>
        throw new HTLCUpdateFailure(s"Error fulfilling HTLC ${htlcID} on channel ${channelID}: ${error}")
    }

    nextHops match {
      case (nextChannelInfo, nextHtlcID) :: _ =>
        // Intermediate hop in the circuit.
        val maybeError = channels.get(nextChannelInfo.id) match {
          case Some(nextChannel) => nextChannel.failRemoteHTLC(nextHtlcID).left.toOption
          case None => Some(UnknownNextPeer)
        }
        maybeError.foreach(error => throw new HTLCUpdateFailure(
          s"Error forwarding HTLC ${nextHtlcID} fulfill on channel ${nextChannelInfo.id}: $error"
        ))

        val newRoute = BackwardRoutingPacket(nextHops)
        actions.sendMessage(HTLCUpdateProcessingTime, nextChannelInfo.source, UpdateFulfillHTLC(newRoute))

      case Nil =>
        // Payment confirmation made it back to the original sender.
        completePayment(htlc.paymentID, actions.timestamp)
    }
  }

  def handleOpenChannel(otherNode: NodeID, message: OpenChannel)
                       (implicit actions: NodeActions): Unit = {
    // TODO: Sanity check params
    val acceptMsg = AcceptChannel(message, params.requiredConfirmations, params.channelParams(message.capacity))
    actions.sendMessage(OpenChannelProcessingTime, otherNode, acceptMsg)
  }

  def handleAcceptChannel(otherNode: NodeID, acceptMsg: AcceptChannel)
                         (implicit actions: NodeActions): Unit = {
    // TODO: Sanity check params
    val openMsg = acceptMsg.openMsg
    val channelID = acceptMsg.openMsg.id

    // The initiating node broadcasts the funding transactions to the blockchain.
    blockchain.newFundingTransaction(channelID)

    channelOpen(
      channelID = openMsg.id,
      otherNode = otherNode,
      requiredConfirmations = acceptMsg.minimumDepth,
      localBalance = openMsg.capacity - openMsg.pushAmount,
      remoteBalance = openMsg.pushAmount,
      localParams = openMsg.params,
      remoteParams = acceptMsg.params
    )
    actions.sendMessage(InsignificantTimeDelta, otherNode, FundingCreated(channelID, acceptMsg))
  }

  def handleFundingCreated(otherNode: NodeID, message: FundingCreated)
                          (implicit actions: NodeActions): Unit = {
    val acceptMsg = message.acceptMsg
    val openMsg = acceptMsg.openMsg
    val channelID = openMsg.id
    channelOpen(
      channelID = channelID,
      otherNode = otherNode,
      requiredConfirmations = acceptMsg.minimumDepth,
      localBalance = openMsg.pushAmount,
      remoteBalance = openMsg.capacity - openMsg.pushAmount,
      localParams = acceptMsg.params,
      remoteParams = openMsg.params
    )
  }

  private def acceptHTLC(htlc: HTLC, finalHop: FinalHop): Option[RoutingError] = {
    if (!finalHop.paymentIDKnown) {
      return Some(UnknownPaymentHash)
    }
    if (htlc.amount < finalHop.amount) {
      return Some(FinalIncorrectHTLCAmount(htlc.amount))
    }
    if (htlc.expiry < finalHop.expiry) {
      return Some(FinalIncorrectExpiry(htlc.expiry))
    }
    if (finalHop.expiry < blockchain.blockNumber + params.finalExpiryDelta) {
      return Some(FinalExpiryTooSoon)
    }
    None
  }

  def route(paymentInfo: PaymentInfo,
            constraints: RouteConstraints,
            paymentIDKnown: Boolean): Option[ForwardRoutingPacket] = {
    val pathIterator = router.findPath(paymentInfo, graphView, constraints)
    if (pathIterator.isEmpty) {
      return None
    }

    val finalHop: FinalHop = FinalHop(
      amount = paymentInfo.amount,
      expiry = blockchain.blockNumber + paymentInfo.finalExpiryDelta,
      paymentIDKnown = paymentIDKnown
    )

    var hops: List[HTLC] = Nil
    var amount = finalHop.amount
    var expiry = finalHop.expiry

    val path = pathIterator.zipWithIndex
    for ((edge, i) <- path.reverseIterator.toIndexedSeq) {
      hops = HTLC(edge, HTLC.Desc(-1, amount, expiry, paymentInfo.paymentID)) :: hops
      amount += edge.fee(amount)
      expiry += edge.expiryDelta
    }

    Some(ForwardRoutingPacket(hops, finalHop, BackwardRoutingPacket(Nil)))
  }

  def sendPayment(paymentInfo: PaymentInfo)
                 (implicit actions: NodeActions): Unit = {
    val pendingPayment = PendingPayment(
      paymentInfo,
      actions.timestamp,
      tries = 1,
      constraints = new RouteConstraints(),
    )
    executePayment(pendingPayment)
  }

  def executePayment(pendingPayment: PendingPayment)
                    (implicit actions: NodeActions): Unit = {
    val paymentInfo = pendingPayment.info
    val paymentID = paymentInfo.paymentID
    if (paymentInfo.recipientID == id) {
      logger.warn(
        "msg" -> "Failed attempt to send payment to self".toJson,
        "paymentID" -> paymentID.toJson
      )
      return
    }
    if (pendingPayments.contains(paymentID)) {
      logger.warn(
        "msg" -> "Failed attempt to execute duplicate payment".toJson,
        "paymentID" -> paymentID.toJson
      )
      return
    }

    pendingPayments(paymentID) = pendingPayment

    route(paymentInfo, pendingPayment.constraints, paymentIDKnown = true) match {
      // Attempt to send payment through the Lightning Network if a route is found.
      case Some(routingPacket) =>
        val (firstHop :: restHops) = routingPacket.hops
        logger.debug(
          "msg" -> "Attempting to complete payment through Lightning Network".toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )

        sendHTLC(firstHop) match {
          case Left(error) =>
            failPayment(firstHop.paymentID, error, Some(firstHop.channel))
          case Right(firstHTLC) =>
            val newRoute = routingPacket.copy(hops = firstHTLC :: restHops)
            actions.sendMessage(RoutingTime, firstHop.recipient, UpdateAddHTLC(newRoute))
        }

      // Otherwise, open a channel directly to the peer
      case None =>
        val capacity = newChannelCapacity(paymentInfo.recipientID, paymentInfo.amount)
        val channelID = Util.randomUUID()
        val channelParams = params.channelParams(capacity)
        val openMsg = OpenChannel(channelID, capacity, paymentInfo.amount, channelParams)

        logger.debug(
          "msg" -> "Attempting to complete payment by opening new direct channel".toJson,
          "channelID" -> channelID.toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )
        actions.sendMessage(RoutingTime, paymentInfo.recipientID, openMsg)
    }
  }

  private def sendHTLC(htlc: HTLC): Either[RoutingError, HTLC] = {
    channels.get(htlc.channel.id) match {
      case Some(channel) =>
        // TODO: Check CLTV delta + fee rate against channel update params
        val htlcDesc = htlc.desc.copy(id = channel.ourNextHTLCID)
       channel.addLocalHTLC(htlcDesc)
          .map(_ match {
            case ChannelView.Error.IncorrectHTLCID =>
              throw new AssertionError("HTLC should have been correctly assigned in call to addLocalHTLC")
            case ChannelView.Error.BelowHTLCMinimum =>
              AmountBelowMinimum(htlc.amount)
            case ChannelView.Error.InsufficientBalance |
                 ChannelView.Error.ExceedsMaxHTLCInFlight |
                 ChannelView.Error.ExceedsMaxAcceptedHTLCs =>
              TemporaryChannelFailure
          })
          .toLeft(HTLC(htlc.channel, htlcDesc))
      case None => Left(UnknownNextPeer)
    }
  }

  private def newChannelCapacity(_node: NodeID, initialPaymentAmount: Value): Value =
    initialPaymentAmount * CapacityMultiplier

  private def channelUpdate(channelID: ChannelID,
                            otherNodeID: NodeID,
                            timestamp: Timestamp,
                            disabled: Boolean,
                            capacity: Value,
                            theirChannelParams: ChannelParams): Channel = {
    Channel(
      id = channelID,
      source = id,
      target = otherNodeID,
      lastUpdate = timestamp,
      disabled = disabled,
      expiryDelta = params.expiryDelta,
      htlcMinimum = theirChannelParams.htlcMinimum,
      htlcMaximum = math.min(theirChannelParams.maxHTLCInFlight, capacity),
      feeBase = params.feeBase,
      feeProportionalMillionths = params.feeProportionalMillionths
    )
  }

  private def completePayment(paymentID: PaymentID, timestamp: Timestamp): Unit = {
    val pendingPayment = pendingPayments.remove(paymentID)
      .getOrElse(throw new AssertionError(s"Unknown payment ${paymentID} failed"))

    logger.info(
      "msg" -> "Payment completed".toJson,
      "paymentID" -> paymentID.toJson,
      "tries" -> pendingPayment.tries.toJson,
      "totalTime" -> (timestamp - pendingPayment.timestamp).toJson,
    )
  }

  private def failPayment(paymentID: PaymentID, error: RoutingError, channel: Option[Channel])
                         (implicit actions: NodeActions): Unit = {
    val pendingPayment = pendingPayments.remove(paymentID)
      .getOrElse(throw new AssertionError(s"Unknown payment ${paymentID} failed"))

    logger.debug(
      "msg" -> "Payment failed".toJson,
      "paymentID" -> paymentID.toJson,
      "tries" -> pendingPayment.tries.toJson,
      "channelID" -> channel.map(_.id.toJson).getOrElse(JsNull),
      "error" -> error.toString.toJson,
    )

    var permanentIgnoreNode = Option.empty[NodeID]
    var permanentIgnoreChannel = Option.empty[Channel]
    var temporaryIgnoreNode = Option.empty[NodeID]
    var temporaryIgnoreChannel = Option.empty[Channel]

    channel match {
      case Some(channel) => error match {
        case TemporaryChannelFailure => temporaryIgnoreChannel = Some(channel)
        case _: NodeError => error match {
          case _: PermanentError => permanentIgnoreNode = Some(channel.source)
          case _ => temporaryIgnoreNode = Some(channel.source)
        }
        case _: PermanentError => permanentIgnoreChannel = Some(channel)
        case _: UpdateError => // TODO: Validate error legitimacy
        case _: BadOnionError => ???
        case FinalExpiryTooSoon |
             FinalIncorrectExpiry(_) |
             FinalIncorrectHTLCAmount(_) |
             NotDecodable =>
          throw new MisbehavingNodeException(s"$error error received with a non-empty channel")
      }
      case None => error match {
        case FinalExpiryTooSoon => // TODO: Validate against timestamp
        // TODO: Handle corrupted routing errors.
        case NotDecodable => ???
        case _ => logger.warn(
          "msg" -> "Payment failed permanently at receiving node".toJson,
          "paymentID" -> paymentID.toJson,
          "error" -> error.toString.toJson,
        )
      }
    }

    // Update global constraints
    permanentIgnoreNode.foreach(graphView.banNode(_))
    permanentIgnoreChannel.foreach(graphView.banChannel(_))

    // Update temporary constraints
    var newConstraints = pendingPayment.constraints
    newConstraints = temporaryIgnoreNode.foldLeft(newConstraints) { _.banNode(_) }
    newConstraints = temporaryIgnoreChannel.foldLeft(newConstraints) { _.banChannel(_) }

    val newPendingPayment = pendingPayment.copy(
      tries = pendingPayment.tries + 1,
      constraints = newConstraints,
    )
    actions.retryPayment(PaymentRetryDelay, newPendingPayment)
  }
}

object NodeActor {
  val RoutingTime: TimeDelta = 100
  val HTLCUpdateProcessingTime: TimeDelta = 10
  val OpenChannelProcessingTime = 1
  val InsignificantTimeDelta = 1
  val CapacityMultiplier = 4
  val PaymentRetryDelay = 1000

  case class Params(reserveToFundingRatio: Double,
                    dustLimit: Value,
                    maxHTLCInFlight: Value,
                    maxAcceptedHTLCs: Int,
                    htlcMinimum: Value,
                    requiredConfirmations: BlockDelta,
                    finalExpiryDelta: BlockDelta,
                    expiryDelta: BlockDelta,
                    feeBase: Value,
                    feeProportionalMillionths: Long) {

    def channelParams(capacity: Value): ChannelParams =
      ChannelParams(
        (capacity * reserveToFundingRatio).toLong,
        dustLimit,
        maxHTLCInFlight,
        maxAcceptedHTLCs,
        htlcMinimum
      )
  }
}
