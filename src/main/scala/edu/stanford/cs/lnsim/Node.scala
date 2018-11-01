package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.des.TimeDelta

class Node(val id: NodeID, private val behavior: NodeBehavior) {
  def this(behavior: NodeBehavior) = this(UUID.randomUUID(), behavior)

  def meanNetworkLatency: Double = 1

  def route(paymentInfo: PaymentInfo): RoutingPacket = behavior.route(paymentInfo)

  def forwardHTLC(hop: HTLC, nextHop: HTLC): (TimeDelta, Option[RoutingError]) = behavior.forwardHTLC(hop, nextHop)

  def acceptHTLC(hop: HTLC, finalHop: FinalHop): (TimeDelta, Option[RoutingError]) = behavior.acceptHTLC(hop, finalHop)

  override def toString: String = s"Node($id)"
}

