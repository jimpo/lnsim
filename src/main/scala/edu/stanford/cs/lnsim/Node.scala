package edu.stanford.cs.lnsim

import java.util.UUID

class Node(val id: UUID, private val behavior: NodeBehavior) {
  def this(behavior: NodeBehavior) = this(UUID.randomUUID(), behavior)
  def meanNetworkLatency: Double = 1

  def route(paymentInfo: PaymentInfo): Array[HTLC] = behavior.route(paymentInfo)

  override def toString: String = s"Node($id)"
}

