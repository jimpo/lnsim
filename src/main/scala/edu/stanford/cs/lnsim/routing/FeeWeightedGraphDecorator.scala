package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim._
import org.jgrapht.Graph

class FeeWeightedGraphDecorator(target: Graph[NodeID, ChannelWithDirection], paymentAmount: Value)
  extends GraphDecorator[NodeID, ChannelWithDirection](target) {

  override def getEdgeWeight(edge: ChannelWithDirection): Double = {
    val channelUpdate = edge.channelUpdate
    channelUpdate.feeBase + paymentAmount * channelUpdate.feeProportionalMillionths / 1000000.0
  }
}
