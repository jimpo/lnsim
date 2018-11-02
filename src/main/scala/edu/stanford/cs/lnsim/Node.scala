package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.collection.mutable

class Node(val id: NodeID, private val behavior: NodeBehavior) {
  import Node._

  private val packetQueue: mutable.Queue[(TimeDelta, Node, Message)] = new mutable.Queue()

  def this(behavior: NodeBehavior) = this(UUID.randomUUID(), behavior)

  def meanNetworkLatency: Double = 1

  def route(paymentInfo: PaymentInfo): RoutingPacket = behavior.route(paymentInfo)

  def forwardHTLC(hop: HTLC, nextHop: HTLC): (TimeDelta, Option[RoutingError]) = behavior.forwardHTLC(hop, nextHop)

  def failHTLC(hop: HTLC): TimeDelta = behavior.failHTLC(hop)

  def acceptHTLC(hop: HTLC, finalHop: FinalHop): (TimeDelta, Option[RoutingError]) = behavior.acceptHTLC(hop, finalHop)

  def failPayment(hop: HTLC, error: RoutingError): Unit = behavior.failPayment(hop, error)

  def handleUpdateAddHTLC(sender: Node, message: UpdateAddHTLC): Unit = {
    val UpdateAddHTLC(route, index) = message
    val hop = route.hops(index)
    val node = hop.recipient

    if (index + 1 < route.hops.length) {
      // Intermediate hop in the circuit.
      val nextHop = route.hops(index + 1)
      val (delay: TimeDelta, maybeError) = if (nextHop.sender != node) {
        (FailedHTLCProcessingTime, Some(UnknownNextPeer))
      } else {
        forwardHTLC(hop, nextHop)
      }

      val event = maybeError match {
        case Some(error) =>
          packetQueue.enqueue((delay, sender, UpdateFailHTLC(route, index, error)))
        case None =>
          packetQueue.enqueue((delay, nextHop.recipient, UpdateAddHTLC(route, index + 1)))
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

      if (index > 0) {
        // Intermediate hop in the circuit.
        val nextHop = route.hops(index - 1)

        val delay = node.failHTLC(hop)
        packetQueue.enqueue((delay, nextHop.recipient, UpdateFailHTLC(route, index - 1, error)))
      } else {
        // Error made it back to the original sender.
        node.failPayment(hop, error)
      }
  }

  def handleUpdateFulfillHTLC(sender: Node, message: UpdateFulfillHTLC): Unit = {
    val UpdateFulfillHTLC(route, index) = message
  }

  def drainPacketQueue(): Seq[(TimeDelta, Node, Message)] = packetQueue.dequeueAll((_) => true)

  override def toString: String = s"Node($id)"
}

object Node {
  val FailedHTLCProcessingTime = 10
}
