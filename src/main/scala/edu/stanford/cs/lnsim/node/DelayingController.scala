package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.node.DelayingController._
import edu.stanford.cs.lnsim.routing.NetworkGraphView
import edu.stanford.cs.lnsim.{BlockDelta, NodeID, Value}

import scala.util.Random

class DelayingController(feeBase: Value,
                         feeProportionalMillionths: Long,
                         finalExpiryDelta: BlockDelta,
                         requiredExpiryDelta: BlockDelta,
                         minExpiry: BlockDelta,
                         maxExpiry: BlockDelta,
                         private val attackParams: AttackParams)
  extends DefaultController(
          feeBase,
          feeProportionalMillionths,
          finalExpiryDelta,
          requiredExpiryDelta,
          minExpiry,
          maxExpiry) {

  override def autoConnect(sourceNodeID: NodeID,
                           budget: Value,
                           graphView: NetworkGraphView): Seq[(NodeID, Value)] = {
    val nodes = Random.shuffle(graphView.nodeIterator)

    var remainingBudget = budget
    var channels: List[(NodeID, Value)] = Nil
    for (target <- nodes) {
      if (remainingBudget >= attackParams.channelCapacity) {
        return channels
      }
      if (target.id != sourceNodeID) {
        channels = (target.id, attackParams.channelCapacity) :: channels
      }
    }
    channels
  }
}

object DelayingController {
  case class AttackParams(numChannels: Int,
                          channelCapacity: Value)
}
