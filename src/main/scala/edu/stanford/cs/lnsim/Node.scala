package edu.stanford.cs.lnsim

import java.util.UUID

class Node(private val behavior: NodeBehavior) {
  val id: NodeID = UUID.randomUUID()

  def meanNetworkLatency: Double = 1
}

