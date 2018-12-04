package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.routing.NetworkGraphView
import edu.stanford.cs.lnsim.{BlockDelta, NodeID, Util, Value}

class LndAutopilotController(feeBase: Value,
                             feeProportionalMillionths: Long,
                             finalExpiryDelta: BlockDelta,
                             requiredExpiryDelta: BlockDelta,
                             minExpiry: BlockDelta,
                             maxExpiry: BlockDelta,
                             private val autoPilotMinChannelSize: Value,
                             private val autoPilotNumChannels: Int)
  extends DefaultController(
          feeBase,
          feeProportionalMillionths,
          finalExpiryDelta,
          requiredExpiryDelta,
          minExpiry,
          maxExpiry) {

  /** This algorithm is based off of LND's autopilot heuristic, which uses preferential attachment
    * to determine which nodes to open channels to. The algorithm is to:
    *
    * Sample a node at random
    *   - that we do not already have a channel with
    *   - that is not banned
    *   - weighted by the number of active channels it has with other nodes
    *
    * Attempt to open a channel up to some maximum capacity. Repeat until the budget becomes less
    * than the configured minimum channel size.
    *
    * See https://github.com/lightningnetwork/lnd/blob/v0.5-beta/autopilot/prefattach.go#L145
    */
  override def autoConnect(sourceNodeID: NodeID,
                           budget: Value,
                           graphView: NetworkGraphView): Seq[(NodeID, Value)] = {
    // Count the number of channels for each non-banned node. These are the weights of the
    // PMF (probability mass function) of the distribution.
    //
    // TODO: Only open channels to nodes we do not already have connections or pending connections to.
    val nodeWeights = graphView.nodeIterator
      .filter(node => graphView.constraints.allowNode(node.id))
      .map(node => node.id -> node.channels.size)
      .toArray

    // Convert the PMF to a CMF (cumulative mass function) in place.
    for (i <- 1 until nodeWeights.length) {
      val (nodeID, weight) = nodeWeights(i)
      nodeWeights(i) = (nodeID, weight + nodeWeights(i - 1)._2)
    }

    val channelOpenAmount = Math.max(budget / autoPilotNumChannels, autoPilotMinChannelSize)

    var remainingBudget = budget
    var channels: List[(NodeID, Value)] = Nil
    while (remainingBudget >= autoPilotMinChannelSize) {
      val targetNodeID = Util.sampleCMF(nodeWeights)
      if (targetNodeID != sourceNodeID) {
        val capacity = Math.min(remainingBudget, channelOpenAmount)

        channels = (targetNodeID, capacity) :: channels
        remainingBudget -= capacity
      }
    }
    channels
  }
}
