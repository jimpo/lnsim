package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.graph.{Channel, ChannelUpdate}
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.routing.{NetworkGraphView, Router}

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

  def meanNetworkLatency: Double = 1

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

  def handleChannelOpenedOnChain(channelID: ChannelID, timestamp: Timestamp)
                                (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    channels.get(channelID) match {
      case Some(channelView) =>
        channelView.transition(ChannelView.Status.Active)
        val update = channelUpdate(
          timestamp,
          disabled = false,
          capacity = channelView.ourInitialBalance + channelView.theirInitialBalance,
          theirChannelParams = channelView.theirParams
        )
        graphView.updateChannel(Channel(channelID, id, channelView.otherNode, update))

      case None =>
        throw new AssertionError(s"Cannot handle ChannelOpened for unknown channel $channelID")
    }
  }

  def handleUpdateAddHTLC(sender: NodeID, message: UpdateAddHTLC)
                         (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    val UpdateAddHTLC(route, index) = message
    val hop = route.hops(index)
    val node = hop.recipient

    val channel = channels.getOrElse(
      hop.channel.id,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel ${hop.channel.id}")
    )

    // The simulation environment makes the assumption that HTLC updates are atomic for the sake of simplicity,
    // so if an HTLC was added without error on the other end of the channel, there should be no error here either.
    channel.addRemoteHTLC(hop.desc) match {
      case Some(error) =>
        throw new HTLCUpdateFailure(s"Error receiving HTLC ${hop.id} on channel ${hop.channel.id}: ${error}")
      case None =>
    }

    if (index + 1 < route.hops.length) {
      // Intermediate hop in the circuit.
      val nextHop = route.hops(index + 1)
      val maybeError = channels.get(nextHop.channel.id) match {
        case Some(nextChannel) =>
          // TODO: Check CLTV delta + fee rate against channel update params
          nextChannel.addLocalHTLC(nextHop.desc.copy(id = nextChannel.ourNextHTLCID))
            .flatMap(_ match {
              case ChannelView.Error.IncorrectHTLCID =>
                throw new AssertionError("HTLC should have been correctly assigned in call to addLocalHTLC")
              case ChannelView.Error.BelowHTLCMinimum =>
                Some(AmountBelowMinimum(hop.amount))
              case ChannelView.Error.InsufficientBalance |
                   ChannelView.Error.ExceedsMaxHTLCInFlight |
                   ChannelView.Error.ExceedsMaxAcceptedHTLCs =>
                Some(AmountBelowMinimum(hop.amount))
            })
        case None => Some(UnknownNextPeer)
      }

      val event = maybeError match {
        case Some(error) =>
          sendMessage(HTLCUpdateProcessingTime, sender, UpdateFailHTLC(route, index, error))
        case None =>
          sendMessage(HTLCUpdateProcessingTime, nextHop.recipient, UpdateAddHTLC(route, index + 1))
      }
    } else {
      // Final hop in the circuit.
      val maybeError = acceptHTLC(hop, route.finalHop)
      val delay = HTLCUpdateProcessingTime
      maybeError match {
        case Some(error) => sendMessage(delay, sender, UpdateFailHTLC(route, index, error))
        case None => sendMessage(delay, sender, UpdateFulfillHTLC(route, index))
      }
    }
  }

  def handleUpdateFailHTLC(sender: NodeID, message: UpdateFailHTLC)
                          (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    val UpdateFailHTLC(route, index, error) = message
    val hop = route.hops(index)
    val node = hop.sender

    val channel = channels.getOrElse(
      hop.channel.id,
      throw new HTLCUpdateFailure(s"Cannot receive HTLC on unknown channel ${hop.channel.id}")
    )

    channel.failRemoteHTLC(hop.id) match {
      case Some(error) =>
        throw new HTLCUpdateFailure(s"Error failing HTLC ${hop.id} on channel ${hop.channel.id}: $error")
      case None =>
    }

    if (index > 0) {
      // Intermediate hop in the circuit.
      val nextHop = route.hops(index - 1)

      val maybeError = channels.get(nextHop.channel.id) match {
        case Some(nextChannel) => nextChannel.failLocalHTLC(nextHop.id)
        case None => Some(UnknownNextPeer)
      }
      maybeError match {
        case Some(error) =>
          throw new HTLCUpdateFailure(s"Error forwarding HTLC ${hop.id} fail on channel ${hop.channel.id}: $error")
        case None =>
      }

      val delay = HTLCUpdateProcessingTime
      sendMessage(delay, nextHop.recipient, UpdateFailHTLC(route, index - 1, error))
    } else {
      // Error made it back to the original sender.
      // node.failPayment(hop, error)
    }
  }

  def handleUpdateFulfillHTLC(sender: NodeID, message: UpdateFulfillHTLC)
                             (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    val UpdateFulfillHTLC(route, index) = message
    val hop = route.hops(index)
    val node = hop.sender

    val channel = channels.getOrElse(
      hop.channel.id,
      throw new HTLCUpdateFailure(s"Cannot fulfill HTLC on unknown channel ${hop.channel.id}")
    )

    channel.fulfillRemoteHTLC(hop.id) match {
      case Some(error) =>
        throw new HTLCUpdateFailure(s"Error fulfilling HTLC ${hop.id} on channel ${hop.channel.id}: ${error}")
      case None =>
    }

    if (index > 0) {
       // Intermediate hop in the circuit.
      val nextHop = route.hops(index - 1)

      val maybeError = channels.get(nextHop.channel.id) match {
        case Some(nextChannel) => nextChannel.failLocalHTLC(nextHop.id)
        case None => Some(UnknownNextPeer)
      }
      maybeError match {
        case Some(error) =>
          throw new HTLCUpdateFailure(s"Error forwarding HTLC ${hop.id} fulfill on channel ${hop.channel.id}: $error")
        case None =>
      }

      sendMessage(HTLCUpdateProcessingTime, nextHop.recipient, UpdateFulfillHTLC(route, index - 1))
    } else {
      // Payment confirmation made it back to the original sender.
    }
  }

  def handleOpenChannel(otherNode: NodeID, message: OpenChannel)
                       (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    // TODO: Sanity check params
    logger.info(
      "msg" -> "handleOpenChannel".toJson,
      "thisNode" -> id.toJson,
      "otherNode" -> otherNode.toJson
    )
    val acceptMsg = AcceptChannel(message, params.requiredConfirmations, params.channelParams(message.capacity))
    sendMessage(OpenChannelProcessingTime, otherNode, acceptMsg)
  }

  def handleAcceptChannel(otherNode: NodeID, acceptMsg: AcceptChannel)
                         (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    // TODO: Sanity check params
    val openMsg = acceptMsg.openMsg
    val channelID = acceptMsg.openMsg.id

    // The initiating node broadcasts the funding transactions to the blockchain.
    blockchain.newFundingTransaction(channelID)

    logger.info(
      "msg" -> "Channel was accepted".toJson,
      "thisNode" -> id.toJson,
      "otherNode" -> otherNode.toJson
    )
    channelOpen(
      channelID = openMsg.id,
      otherNode = otherNode,
      requiredConfirmations = acceptMsg.minimumDepth,
      localBalance = openMsg.capacity - openMsg.pushAmount,
      remoteBalance = openMsg.pushAmount,
      localParams = openMsg.params,
      remoteParams = acceptMsg.params
    )
    sendMessage(InsignificantTimeDelta, otherNode, FundingCreated(channelID, acceptMsg))
  }

  def handleFundingCreated(otherNode: NodeID, message: FundingCreated)
                          (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
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

  /*
  def channel(id: ChannelID): Option[Channel] = channels.get(id).map(_.channel)
  def channelIterator: Iterator[Channel] = channels.valuesIterator.map(_.channel)
 */

  def route(paymentInfo: PaymentInfo, paymentIDKnown: Boolean): Option[RoutingPacket] = {
    val pathIterator = router.findPath(paymentInfo, graphView)
    if (pathIterator.isEmpty) {
      return None
    }

    val path = pathIterator.zipWithIndex.toList
    val hops = Array.ofDim[HTLC](path.length)
    val finalHop: FinalHop = FinalHop(
      amount = paymentInfo.amount,
      expiry = blockchain.blockNumber + paymentInfo.finalExpiryDelta,
      paymentIDKnown = paymentIDKnown
    )

    var amount = finalHop.amount
    var expiry = finalHop.expiry
    for ((edge, i) <- path.reverseIterator.toIndexedSeq) {
      val channelUpdate = edge.lastUpdate
      hops(i) = HTLC(edge, HTLC.Desc(-1, amount, expiry, paymentInfo.paymentID))
      amount += channelUpdate.feeBase + amount * channelUpdate.feeProportionalMillionths / 1000000
      expiry += channelUpdate.expiryDelta
    }

    Some(RoutingPacket(hops, finalHop))
  }

  def sendPayment(paymentInfo: PaymentInfo)
                 (implicit sendMessage: (TimeDelta, NodeID, Message) => Unit): Unit = {
    if (paymentInfo.recipientID == id) {
      logger.warn(
        "msg" -> "Failed attempt to send payment to self".toJson,
        "paymentID" -> paymentInfo.paymentID.toJson
      )
      return
    }
    if (paymentInfo.recipientID.equals(id)) {
      logger.warn(
        "msg" -> "Well, that was unexpected".toJson,
        "paymentID" -> paymentInfo.paymentID.toJson
      )
      return
    }

    route(paymentInfo, paymentIDKnown = true) match {
      // Attempt to send payment through the Lightning Network if a route is found.
      case Some(routingPacket) =>
        val firstHop = routingPacket.hops.head

        logger.debug(
          "msg" -> "Attempting to complete payment through Lightning Network".toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )
        sendMessage(RoutingTime, firstHop.recipient, UpdateAddHTLC(routingPacket, 0))

      // Otherwise, open a channel directly to the peer
      case None =>
        val capacity = newChannelCapacity(paymentInfo.recipientID, paymentInfo.amount)
        val channelID = Util.randomUUID()
        val channelParams = params.channelParams(capacity)
        val openMsg = OpenChannel(channelID, capacity, paymentInfo.amount, channelParams)

        logger.info(
          "msg" -> "Attempting to complete payment by opening new direct channel".toJson,
          "channelID" -> channelID.toJson,
          "paymentID" -> paymentInfo.paymentID.toJson
        )
        sendMessage(RoutingTime, paymentInfo.recipientID, openMsg)
    }
  }

  private def newChannelCapacity(_node: NodeID, initialPaymentAmount: Value): Value =
    initialPaymentAmount * CapacityMultiplier

  private def channelUpdate(timestamp: Timestamp,
                            disabled: Boolean,
                            capacity: Value,
                            theirChannelParams: ChannelParams): ChannelUpdate = {
    ChannelUpdate(
      timestamp,
      disabled,
      expiryDelta = params.expiryDelta,
      htlcMinimum = theirChannelParams.htlcMinimum,
      htlcMaximum = math.min(theirChannelParams.maxHTLCInFlight, capacity),
      feeBase = params.feeBase,
      feeProportionalMillionths = params.feeProportionalMillionths
    )
  }
}

object NodeActor {
  val HTLCUpdateProcessingTime = 10
  val RoutingTime = 10
  val OpenChannelProcessingTime = 1
  val InsignificantTimeDelta = 1
  val CapacityMultiplier = 4

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
