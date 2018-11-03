package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.collection.mutable

class Node(val id: NodeID, private val behavior: NodeBehavior) {
  import Node._

  private val channels: mutable.Map[ChannelID, ChannelView] = mutable.HashMap.empty
  private val packetQueue: mutable.Queue[(TimeDelta, Node, Message)] = mutable.Queue.empty

  def this(behavior: NodeBehavior) = this(UUID.randomUUID(), behavior)

  def meanNetworkLatency: Double = 1

  def route(paymentInfo: PaymentInfo): RoutingPacket = behavior.route(paymentInfo)

  def forwardHTLC(hop: HTLC, nextHop: HTLC): (TimeDelta, Option[RoutingError]) = behavior.forwardHTLC(hop, nextHop)

  def failHTLC(hop: HTLC): TimeDelta = behavior.failHTLC(hop)

  def acceptHTLC(hop: HTLC, finalHop: FinalHop): (TimeDelta, Option[RoutingError]) = behavior.acceptHTLC(hop, finalHop)

  def failPayment(hop: HTLC, error: RoutingError): Unit = behavior.failPayment(hop, error)

  def handleUpdateAddHTLC(sender: Node, message: UpdateAddHTLC, blockNumber: BlockNumber): Unit = {
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
          packetQueue.enqueue((FailedHTLCProcessingTime, sender, UpdateFailHTLC(route, index, error)))
        case None =>
          packetQueue.enqueue((HTLCUpdateProcessingTime, nextHop.recipient, UpdateAddHTLC(route, index + 1)))
      }
    } else {
      // Final hop in the circuit.
      val (delay, maybeError) = node.acceptHTLC(hop, route.finalHop)
      maybeError match {
        case Some(error) => packetQueue.enqueue((delay, sender, UpdateFailHTLC(route, index, error)))
        case None => packetQueue.enqueue((delay, sender, UpdateFulfillHTLC(route, index)))
      }
    }
  }

  def handleUpdateFailHTLC(sender: Node, message: UpdateFailHTLC): Unit = {
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

      val delay = node.failHTLC(hop)
      packetQueue.enqueue((delay, nextHop.recipient, UpdateFailHTLC(route, index - 1, error)))
    } else {
      // Error made it back to the original sender.
      node.failPayment(hop, error)
    }
  }

  def handleUpdateFulfillHTLC(sender: Node, message: UpdateFulfillHTLC): Unit = {
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

      packetQueue.enqueue((HTLCUpdateProcessingTime, nextHop.recipient, UpdateFulfillHTLC(route, index - 1)))
    } else {
      // Payment confirmation made it back to the original sender.
    }
  }

  def drainPacketQueue(): Seq[(TimeDelta, Node, Message)] = packetQueue.dequeueAll((_) => true)

  override def toString: String = s"Node($id)"
}

object Node {
  val HTLCUpdateProcessingTime = 10
  val FailedHTLCProcessingTime = 10
}
