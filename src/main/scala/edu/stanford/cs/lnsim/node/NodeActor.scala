package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.graph.Channel
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.routing.{NetworkGraphView, RouteConstraints, Router}
import edu.stanford.cs.lnsim.JSONProtocol._
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable

class NodeActor(val id: NodeID,
                val params: NodeActor.Params,
                private val controller: NodeController,
                private val output: ObservableOutput,
                private val router: Router,
                private val graphView: NetworkGraphView,
                private val blockchain: BlockchainView) extends StructuredLogging {

  import NodeActor._

  private val channels: mutable.Map[ChannelID, ChannelView] = mutable.HashMap.empty
  private val pendingPayments: mutable.Map[PaymentID, PendingPayment] = mutable.HashMap.empty
  private val onChainPayments: mutable.Map[ChannelID, PaymentID] = mutable.HashMap.empty

  def meanNetworkLatency: TimeDelta = 100

  protected def lookupChannel(channelID: ChannelID): Option[ChannelView] = channels.get(channelID)

  def channelOpen(channelID: ChannelID,
                  otherNode: NodeID,
                  requiredConfirmations: BlockDelta,
                  localBalance: Value,
                  remoteBalance: Value,
                  localParams: ChannelParams,
                  remoteParams: ChannelParams): Unit = {
    if (channels.contains(channelID)) {
      throw new AssertionError(s"Channel $channelID has already been added to node $id")
    }

    channels(channelID) = new ChannelView(otherNode, localBalance, remoteBalance, localParams, remoteParams)
    blockchain.subscribeChannelConfirmed(channelID, requiredConfirmations)
  }

  def handleChannelOpenedOnChain(channelID: ChannelID)
                                (implicit ctx: NodeContext): Unit = {
    lookupChannel(channelID) match {
      case Some(channelView) =>
        channelView.transition(ChannelView.Status.Active)
        val channel = channelUpdate(
          channelID = channelID,
          otherNodeID = channelView.otherNode,
          timestamp = ctx.timestamp,
          disabled = false,
          capacity = channelView.ourInitialBalance + channelView.theirInitialBalance,
          theirChannelParams = channelView.theirParams
        )
        graphView.updateChannel(channel)

        for (paymentID <- onChainPayments.remove(channelID)) {
          completePayment(paymentID, ctx.timestamp)
        }

      case None =>
        throw new AssertionError(s"Cannot handle ChannelOpened for unknown channel $channelID")
    }
  }

  protected def addRemoteHTLC(htlc: HTLC)
                           (implicit ctx: NodeContext): Unit = {
     val channel = lookupChannel(htlc.channel.id)
       .getOrElse(throw new HTLCUpdateFailure(
         s"Node $id cannot receive HTLC on unknown channel ${htlc.channel.id}"))

    // The simulation environment makes the assumption that HTLC updates are atomic for the sake of simplicity,
    // so if an HTLC was added without error on the other end of the channel, there should be no error here either.
    channel.addRemoteHTLC(htlc.desc) match {
      case Some(error) =>
        throw new HTLCUpdateFailure(s"Error receiving HTLC ${htlc.id} on channel ${htlc.channel.id}: $error")
      case None =>
    }

    ctx.advanceTimestamp(HTLCUpdateProcessingTime)
  }

  def handleUpdateAddHTLC(sender: NodeID, message: UpdateAddHTLC)
                         (implicit ctx: NodeContext): Unit = {
    val UpdateAddHTLC(route) = message
    val hop :: nextHops = route.hops

    addRemoteHTLC(hop)

    val backwardsRoute = BackwardRoutingPacket((hop.channel, hop.id) :: route.backwardRoute.hops)
    nextHops.headOption match {
      case Some(nextHop) =>
        val newRoute = ForwardRoutingPacket(nextHops, route.finalHop, backwardsRoute)
        processIntermediateHopHTLC(hop, newRoute)
      case None =>
        processFinalHopHTLC(hop, route.finalHop, backwardsRoute)
    }
  }

  private def forwardHTLC(route: ForwardRoutingPacket)(implicit ctx: NodeContext): Unit = {
    val nextHop :: restHops = route.hops
    sendHTLC(nextHop) match {
      case Left(error) => failHTLC(route.backwardRoute, error, Some(nextHop.channel))
      case Right(nextHTLC) =>
        ctx.advanceTimestamp(HTLCUpdateProcessingTime)
        val newRoute = route.copy(hops = nextHTLC :: restHops)
        ctx.sendMessage(nextHop.recipient, UpdateAddHTLC(newRoute))
    }
  }

  private def failHTLC(route: BackwardRoutingPacket, error: RoutingError, errorChannel: Option[Channel])
                      (implicit ctx: NodeContext): Unit = {
    val (channel, htlcID) = route.hops.head

    val maybeError = lookupChannel(channel.id) match {
      case Some(channelView) =>
        ctx.advanceTimestamp(HTLCUpdateProcessingTime)
        channelView.failRemoteHTLC(htlcID).left.toOption
      case None => Some(UnknownNextPeer)
    }
    maybeError.foreach(error => throw new HTLCUpdateFailure(
      s"Error failing HTLC $htlcID on channel ${channel.id}: $error"
    ))

    ctx.sendMessage(channel.source, UpdateFailHTLC(route, error, errorChannel))
  }

  private def fulfillHTLC(route: BackwardRoutingPacket)(implicit ctx: NodeContext): Unit = {
    val (channel, htlcID) = route.hops.head

    val maybeError = lookupChannel(channel.id) match {
      case Some(channelView) =>
        ctx.advanceTimestamp(HTLCUpdateProcessingTime)
        channelView.fulfillRemoteHTLC(htlcID).left.toOption
      case None => Some(UnknownNextPeer)
    }
    maybeError.foreach(error => throw new HTLCUpdateFailure(
      s"Error fulfilling HTLC $htlcID on channel ${channel.id}: $error"
    ))

    ctx.sendMessage(channel.source, UpdateFulfillHTLC(route))
  }

  protected def processFinalHopHTLC(incomingHTLC: HTLC, finalHop: FinalHop, backwardsRoute: BackwardRoutingPacket)
                                   (implicit ctx: NodeContext): Unit = {
    val (maybeError, maybeBlockNumber) =
      controller.acceptHTLC(incomingHTLC, finalHop, blockchain.blockNumber)
    val action = maybeError match {
      case Some(error) => FailHTLC(backwardsRoute, error, None)
      case None => FulfillHTLC(backwardsRoute)
    }

    val blockNumber = maybeBlockNumber.getOrElse(blockchain.blockNumber)
    if (!blockchain.subscribeAction(blockNumber, action)) {
      // If we failed to register the subscription, do the action immediately.
      handleAction(action)
    }
  }

  private def processIntermediateHopHTLC(prevHop: HTLC, route: ForwardRoutingPacket)
                                        (implicit ctx: NodeContext): Unit = {
    val nextHop :: restHops = route.hops
    val (maybeError, maybeBlockNumber) =
      controller.forwardHTLC(prevHop, nextHop, blockchain.blockNumber)
    val action = maybeError match {
      case Some(error) =>
        FailHTLC(route.backwardRoute, error, Some(nextHop.channel))
      case None => ForwardHTLC(route)
    }

    val blockNumber = maybeBlockNumber.getOrElse(blockchain.blockNumber)
    if (!blockchain.subscribeAction(blockNumber, action)) {
      // If we failed to register the subscription, do the action immediately.
      handleAction(action)
    }
  }

  def handleUpdateFailHTLC(sender: NodeID, failMsg: UpdateFailHTLC)
                          (implicit ctx: NodeContext): Unit = {
    val UpdateFailHTLC(route, _, _) = failMsg
    val (channelInfo, htlcID) :: nextHops = route.hops
    val channelID = channelInfo.id

    val channel = channels.getOrElse(channelID,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel $channelID")
    )

    ctx.advanceTimestamp(HTLCUpdateProcessingTime)
    val htlc = channel.failLocalHTLC(htlcID) match {
      case Right(result) => result
      case Left(error) => throw new HTLCUpdateFailure(
        s"Error failing HTLC $htlcID on channel $channelID: $error"
      )
    }
    checkChannelClosed(channelID, channel)

    if (nextHops.isEmpty) {
      // Error made it back to the original sender.
      failPayment(htlc.paymentID, failMsg.error, failMsg.channel)
    } else {
      // Intermediate hop in the circuit.
      failHTLC(BackwardRoutingPacket(nextHops), failMsg.error, failMsg.channel)
    }
  }

  def handleUpdateFulfillHTLC(sender: NodeID, fulfillMsg: UpdateFulfillHTLC)
                             (implicit ctx: NodeContext): Unit = {
    val UpdateFulfillHTLC(route) = fulfillMsg
    val (channelInfo, htlcID) :: nextHops = route.hops
    val channelID = channelInfo.id

    val channel = channels.getOrElse(channelID,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel $channelID")
    )

    ctx.advanceTimestamp(HTLCUpdateProcessingTime)
    val htlc = channel.fulfillLocalHTLC(htlcID) match {
      case Right(result) => result
      case Left(error) =>
        throw new HTLCUpdateFailure(s"Error fulfilling HTLC $htlcID on channel $channelID: $error")
    }
    checkChannelClosed(channelID, channel)

    if (nextHops.isEmpty) {
      // Payment confirmation made it back to the original sender.
      completePayment(htlc.paymentID, ctx.timestamp)
    } else {
      // Intermediate hop in the circuit.
      fulfillHTLC(BackwardRoutingPacket(nextHops))
    }
  }

  def handleOpenChannel(otherNode: NodeID, message: OpenChannel)
                       (implicit ctx: NodeContext): Unit = {
    // TODO: Sanity check params
    if (message.capacity < params.minChannelCapacity) {
      throw new MisbehavingNodeException(s"Node $otherNode attempted to open channel below minimum amount")
    }

    val acceptMsg = AcceptChannel(
      message,
      params.requiredConfirmations,
      params.channelParams(message.capacity)
    )

    ctx.advanceTimestamp(InsignificantTimeDelta)
    ctx.sendMessage(otherNode, acceptMsg)
  }

  def handleAcceptChannel(otherNode: NodeID, acceptMsg: AcceptChannel)
                         (implicit ctx: NodeContext): Unit = {
    // TODO: Sanity check params
    val openMsg = acceptMsg.openMsg
    val channelID = acceptMsg.openMsg.id

    // The initiating node broadcasts the funding transaction to the blockchain.
    blockchain.newFundingTransaction(channelID)
    ctx.advanceTimestamp(GenerateFundingTransactionTime)

    channelOpen(
      channelID = openMsg.id,
      otherNode = otherNode,
      requiredConfirmations = acceptMsg.minimumDepth,
      localBalance = openMsg.capacity - openMsg.pushAmount,
      remoteBalance = openMsg.pushAmount,
      localParams = openMsg.params,
      remoteParams = acceptMsg.params
    )
    ctx.sendMessage(otherNode, FundingCreated(channelID, acceptMsg))
  }

  def handleFundingCreated(otherNode: NodeID, message: FundingCreated)
                          (implicit ctx: NodeContext): Unit = {
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
                 (implicit ctx: NodeContext): Unit = {
    val pendingPayment = PendingPayment(
      paymentInfo,
      ctx.timestamp,
      tries = 1,
      hops = 0,
      constraints = new RouteConstraints(),
    )
    executePayment(pendingPayment)
  }

  def executePayment(pendingPayment: PendingPayment)
                    (implicit ctx: NodeContext): Unit = {
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

    val paymentElapsedTime = ctx.timestamp - pendingPayment.timestamp
    val maybeRoutingPacket = if (paymentElapsedTime < params.offChainPaymentTimeout) {
      ctx.advanceTimestamp(RoutingTime)
      route(paymentInfo, pendingPayment.constraints, paymentIDKnown = true)
    } else {
      // If it has been too long since the payment was first attempted, stop trying to deliver it
      // off-chain and open a direct channel.
      None
    }

    maybeRoutingPacket match {
      // Attempt to send payment through the Lightning Network if a route is found.
      case Some(routingPacket) =>
        pendingPayments(paymentID) = pendingPayment.copy(hops = routingPacket.hops.length)

        val firstHop :: restHops = routingPacket.hops
        logger.debug(
          "msg" -> "Attempting to complete payment through Lightning Network".toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )

        sendHTLC(firstHop) match {
          case Left(error) =>
            failPayment(firstHop.paymentID, error, Some(firstHop.channel))
          case Right(firstHTLC) =>
            ctx.advanceTimestamp(HTLCUpdateProcessingTime)
            val newRoute = routingPacket.copy(hops = firstHTLC :: restHops)
            ctx.sendMessage(firstHop.recipient, UpdateAddHTLC(newRoute))
        }

      // Otherwise, open a channel directly to the peer
      case None =>
        pendingPayments(paymentID) = pendingPayment

        val capacity = newChannelCapacity(paymentInfo.recipientID, paymentInfo.amount)
        val channelID = initiateChannelOpen(paymentInfo.recipientID, capacity, Some(pendingPayment))
        logger.debug(
          "msg" -> "Attempting to complete payment by opening new direct channel".toJson,
          "channelID" -> channelID.toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )
    }
  }

  def handleShutdown(sender: NodeID, message: Shutdown)
                    (implicit ctx: NodeContext): Unit = {
    val Shutdown(channelID) = message
    val channel = lookupChannel(channelID)
      .getOrElse(throw new MisbehavingNodeException(
        s"Node $sender attempted to close unknown channel $channelID"))

    if (channel.otherNode != sender) {
      throw new MisbehavingNodeException(
        s"Node $sender attempted to close channel $channelID that it is not a party of")
    }

    if (!channel.isClosing) {
      channel.transition(ChannelView.Status.Closing)
      graphView.removeChannel(channelID)
      ctx.sendMessage(sender, message)

      checkChannelClosed(channelID, channel)
    }
  }

  private def checkChannelClosed(channelID: ChannelID, channel: ChannelView): Unit = {
    if (channel.isClosed) {
      channels.remove(channelID)

      logger.info(
        "msg" -> "Channel closed".toJson,
        "node" -> id.toJson,
        "channel" -> channelID.toJson,
        "finalBalance" -> channel.ourAvailableBalance.toJson,
      )
    }
  }

  protected def initiateChannelOpen(nodeID: NodeID,
                                    capacity: Value,
                                    maybePendingPayment: Option[PendingPayment])
                                   (implicit ctx: NodeContext): ChannelID = {
    val channelID = Util.randomUUID()
    val channelParams = params.channelParams(capacity)
    val pushAmount = maybePendingPayment.map(_.info.amount).getOrElse(0L)
    val openMsg = OpenChannel(channelID, capacity, pushAmount, channelParams)

    output.openChannel(
      channelID = channelID,
      initiatingNode = id,
      receivingNode = nodeID,
      capacity = capacity,
      fee = params.channelOpenWeight * blockchain.feePerWeight,
      paymentID = maybePendingPayment.map(_.info.paymentID),
    )

    // Register payment to be completed when channel is open.
    for (pendingPayment <- maybePendingPayment) {
      onChainPayments(channelID) = pendingPayment.info.paymentID
    }
    ctx.sendMessage(nodeID, openMsg)
    channelID
  }

  private def initiateChannelClose(channelID: ChannelID)
                                  (implicit ctx: NodeContext): Unit = {
    val channel = lookupChannel(channelID)
      .getOrElse(throw new AssertionError(
        s"Node $id attempted to close unknown channel $channelID"))

    logger.debug(
      "msg" -> "Initiating channel close".toJson,
      "node" -> id.toJson,
      "channel" -> channelID.toJson,
    )

    // Do not transition immediately to Closing, which indicates that the other party in the
    // channel is aware that the channel is closing. Just disabled the channel and wait for the
    // shutdown message send back.
    channel.transition(ChannelView.Status.Disabled)
    ctx.sendMessage(channel.otherNode, Shutdown(channelID))
  }

  private def sendHTLC(htlc: HTLC): Either[RoutingError, HTLC] = {
    logger.debug(
      "msg" -> "Send HTLC".toJson,
      "node" -> id.toJson,
      "channel" -> htlc.channelID.toJson,
    )
    channels.get(htlc.channel.id) match {
      case Some(channel) =>
        // TODO: Check CLTV delta + fee rate against channel update params
        val htlcDesc = htlc.desc.copy(id = channel.ourNextHTLCID)
        channel.addLocalHTLC(htlcDesc)
          .map {
            case ChannelView.Error.IncorrectHTLCID =>
              throw new AssertionError("HTLC should have been correctly assigned in call to addLocalHTLC")
            case ChannelView.Error.BelowHTLCMinimum =>
              AmountBelowMinimum(htlc.amount)
            case ChannelView.Error.InsufficientBalance |
                 ChannelView.Error.ExceedsMaxHTLCInFlight |
                 ChannelView.Error.ExceedsMaxAcceptedHTLCs =>
              TemporaryChannelFailure
          }
          .toLeft(HTLC(htlc.channel, htlcDesc))
      case None => Left(UnknownNextPeer)
    }
  }

  private def newChannelCapacity(_node: NodeID, initialPaymentAmount: Value): Value =
    Math.max(initialPaymentAmount * params.capacityMultiplier, params.minChannelCapacity)

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
      .getOrElse(throw new AssertionError(s"Unknown payment $paymentID failed"))
    output.paymentCompleted(pendingPayment, timestamp)
  }

  private def failPayment(paymentID: PaymentID, error: RoutingError, maybeChannel: Option[Channel])
                         (implicit ctx: NodeContext): Unit = {
    val pendingPayment = pendingPayments.remove(paymentID)
      .getOrElse(throw new AssertionError(s"Unknown payment $paymentID failed"))

    logger.debug(
      "msg" -> "Payment failed".toJson,
      "paymentID" -> paymentID.toJson,
      "tries" -> pendingPayment.tries.toJson,
      "channelID" -> maybeChannel.map(_.id.toJson).getOrElse(JsNull),
      "error" -> error.toString.toJson,
    )

    var permanentIgnoreNode = Option.empty[NodeID]
    var permanentIgnoreChannel = Option.empty[Channel]
    var temporaryIgnoreNode = Option.empty[NodeID]
    var temporaryIgnoreChannel = Option.empty[Channel]

    maybeChannel match {
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
             ExpiryTooFar |
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
    permanentIgnoreNode.foreach(graphView.banNode)
    permanentIgnoreChannel.foreach(graphView.banChannel)

    // Update temporary constraints
    var newConstraints = pendingPayment.constraints
    newConstraints = temporaryIgnoreNode.foldLeft(newConstraints) {
      _.banNode(_)
    }
    newConstraints = temporaryIgnoreChannel.foldLeft(newConstraints) {
      _.banChannel(_)
    }

    val newPendingPayment = pendingPayment.copy(
      tries = pendingPayment.tries + 1,
      hops = 0,
      constraints = newConstraints,
    )
    ctx.scheduleAction(PaymentRetryDelay, RetryPayment(newPendingPayment))
  }

  def handleAction(action: NodeAction)
                  (implicit ctx: NodeContext): Unit = action match {
    case RetryPayment(payment) => executePayment(payment)
    case ForwardHTLC(route) => forwardHTLC(route)
    case FailHTLC(route, error, channel) => failHTLC(route, error, channel)
    case FulfillHTLC(route) => fulfillHTLC(route)
    case OpenNewChannels(budget) => openNewChannels(budget)
  }

  def openNewChannels(budget: Value)(implicit ctx: NodeContext): Unit = {
    val channelOpenFee = params.channelOpenWeight * blockchain.feePerWeight
    for ((targetNodeID, capacity) <- controller.autoConnect(id, budget, graphView)) {
      initiateChannelOpen(targetNodeID, capacity, maybePendingPayment = None)
    }
  }

  def handleBootstrapEnd()(implicit ctx: NodeContext): Unit =
    controller.bootstrapEndActions().foreach(handleAction)
}

object NodeActor {
  // These numbers are entirely made up with a little bit of intuition.
  // TODO: Perform time measurements on an actual node.
  val RoutingTime: TimeDelta = 100
  val HTLCUpdateProcessingTime: TimeDelta = 10
  val GenerateFundingTransactionTime: TimeDelta = 100
  val InsignificantTimeDelta: TimeDelta = 1

  val PaymentRetryDelay = 1000

  case class Params(reserveToFundingRatio: Double,
                    dustLimit: Value,
                    maxHTLCInFlight: Value,
                    maxAcceptedHTLCs: Int,
                    htlcMinimum: Value,
                    requiredConfirmations: BlockDelta,
                    expiryDelta: BlockDelta,
                    finalExpiryDelta: BlockDelta,
                    feeBase: Value,
                    feeProportionalMillionths: Long,
                    minChannelCapacity: Value,
                    minExpiry: BlockDelta,
                    maxExpiry: BlockDelta,
                    channelOpenWeight: Int,
                    capacityMultiplier: Int,
                    offChainPaymentTimeout: TimeDelta) {

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
