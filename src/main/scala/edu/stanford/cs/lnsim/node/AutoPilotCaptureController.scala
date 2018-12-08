package edu.stanford.cs.lnsim.node

import AutoPilotCaptureController._
import edu.stanford.cs.lnsim.routing.NetworkGraphView
import edu.stanford.cs.lnsim.{NodeID, Value}

import scala.util.Random

class AutoPilotCaptureController(params: NodeActor.Params,
                                 private val attackParams: AttackParams)
  extends DefaultController(params) {

  override def autoConnect(sourceNodeID: NodeID,
                           budget: Value,
                           graphView: NetworkGraphView): Seq[(NodeID, Value)] = {
    val nodes = graphView.nodeIterator.toArray

    var remainingBudget = budget
    var channels: List[(NodeID, Value)] = Nil
    while (remainingBudget >= attackParams.channelCapacity) {
      val target = nodes(Random.nextInt(nodes.length))
      if (target.id != sourceNodeID) {
        channels = (target.id, attackParams.channelCapacity) :: channels
        remainingBudget -= attackParams.channelCapacity
      }
    }
    channels
  }

  override def bootstrapEndActions(): Seq[NodeAction] = Seq(
    OpenNewChannels(attackParams.numChannels * attackParams.channelCapacity)
  )
}

object AutoPilotCaptureController {
  case class AttackParams(numChannels: Int,
                          channelCapacity: Value)
}
