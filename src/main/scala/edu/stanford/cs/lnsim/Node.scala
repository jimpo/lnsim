package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, secondsToTimeDelta}
import edu.stanford.cs.lnsim.routing.Router

import scala.collection.mutable

class Node(val id: NodeID, private val params: Node.Params, val router: Router) {
  import Node._

  private val channels: mutable.Map[ChannelID, ChannelView] = mutable.HashMap.empty

  def this(params: Node.Params, router: Router) = this(Util.randomUUID(), params, router)

  def meanNetworkLatency: Double = 1

  def channelOpen(channel: Channel, localParams: ChannelParams, remoteParams: ChannelParams): Unit = {
    if (channels.contains(channel.id)) {
      throw new AssertionError(s"Channel ${channel.id} has already been added to node $id")
    }
    channels(channel.id) = new ChannelView(channel, localParams, remoteParams)
  }

  def channelClose(channelID: ChannelID): Unit = {
    if (channels.remove(channelID).isEmpty) {
      throw new AssertionError(s"Channel $channelID is unknown to node $id")
    }
  }

  def handleUpdateAddHTLC(sender: Node, message: UpdateAddHTLC, blockNumber: BlockNumber)
                         (implicit sendMessage: (TimeDelta, Node, Message) => Unit): Unit = {
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
      val maybeError = acceptHTLC(hop, route.finalHop, blockNumber)
      val delay = HTLCUpdateProcessingTime
      maybeError match {
        case Some(error) => sendMessage(delay, sender, UpdateFailHTLC(route, index, error))
        case None => sendMessage(delay, sender, UpdateFulfillHTLC(route, index))
      }
    }
  }

  def handleUpdateFailHTLC(sender: Node, message: UpdateFailHTLC)
                          (implicit sendMessage: (TimeDelta, Node, Message) => Unit): Unit = {
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

  def handleUpdateFulfillHTLC(sender: Node, message: UpdateFulfillHTLC)
                             (implicit sendMessage: (TimeDelta, Node, Message) => Unit): Unit = {
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

  private def acceptHTLC(htlc: HTLC, finalHop: FinalHop, blockNumber: BlockNumber): Option[RoutingError] = {
    if (!finalHop.paymentIDKnown) {
      return Some(UnknownPaymentHash)
    }
    if (htlc.amount < finalHop.amount) {
      return Some(FinalIncorrectHTLCAmount(htlc.amount))
    }
    if (htlc.expiry < finalHop.expiry) {
      return Some(FinalIncorrectExpiry(htlc.expiry))
    }
    if (finalHop.expiry < blockNumber + params.finalExpiryDelta) {
      return Some(FinalExpiryTooSoon)
    }
    None
  }

  def channel(id: ChannelID): Option[Channel] = channels.get(id).map(_.channel)
  def channelIterator: Iterator[Channel] = channels.valuesIterator.map(_.channel)

  def newPayments(): List[PaymentInfo] = List.empty

  /**
    * Return the time delay until the node should next be queried to initiate new payments. This
    * is a lower bound on the next time a payment may be initiated by this node.
    *
    * The naive strategy implemented for now is to query every minute.
    */
  def nextPaymentQuery: TimeDelta = secondsToTimeDelta(60)
}

object Node {
  val HTLCUpdateProcessingTime = 10

  case class Params(finalExpiryDelta: BlockDelta)
}
