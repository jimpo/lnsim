package edu.stanford.cs.lnsim

import java.util.UUID

class Node(val id: UUID, private val behavior: NodeBehavior) {
  def this(behavior: NodeBehavior) = this(UUID.randomUUID(), behavior)
  def meanNetworkLatency: Double = 1
}

