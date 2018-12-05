package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.node.DelayingController._
import edu.stanford.cs.lnsim.routing.NetworkGraphView
import edu.stanford.cs.lnsim.{NodeID, Value}

import scala.util.Random

class DelayingController(params: NodeActor.Params,
                         private val attackParams: AttackParams)
  extends DefaultController(params) {

  override def autoConnect(sourceNodeID: NodeID,
                           budget: Value,
                           graphView: NetworkGraphView): Seq[(NodeID, Value)] = {
    val nodes = Random.shuffle(graphView.nodeIterator)

    var remainingBudget = budget
    var channels: List[(NodeID, Value)] = Nil
    for (target <- nodes if target.id != sourceNodeID) {
      if (remainingBudget < attackParams.channelCapacity) {
        return channels
      }
      channels = (target.id, attackParams.channelCapacity) :: channels
      remainingBudget -= attackParams.channelCapacity
    }
    channels
  }
}

object DelayingController {
  case class AttackParams(numChannels: Int,
                          channelCapacity: Value)
}
