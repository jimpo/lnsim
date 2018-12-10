package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.graph.NetworkGraphView
import edu.stanford.cs.lnsim.{NodeID, Value}

import scala.util.Random

class AutoPilotCapture(private val numChannels: Int, private val channelCapacity: Value) {
  def newChannels(sourceNodeID: NodeID,
                  budget: Value,
                  graphView: NetworkGraphView): Seq[(NodeID, Value)] = {
    val nodes = graphView.nodeIterator.toArray

    var remainingBudget = budget
    var channels: List[(NodeID, Value)] = Nil
    while (remainingBudget >= channelCapacity) {
      val target = nodes(Random.nextInt(nodes.length))
      if (target.id != sourceNodeID) {
        channels = (target.id, channelCapacity) :: channels
        remainingBudget -= channelCapacity
      }
    }
    channels
  }
}
