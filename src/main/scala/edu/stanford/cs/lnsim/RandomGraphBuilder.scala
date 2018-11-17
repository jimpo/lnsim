package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.graph.NetworkGraph
import edu.stanford.cs.lnsim.routing.{MinimalFeeRouter, NetworkGraphView}

import scala.util.Random

/**
  * Extremely dumb graph builder that creates channels between random nodes
  * of a fixed capacity, where the initial channel balance is even.
  *
  * @param rand
  */
class RandomGraphBuilder(private val numNodes: Int,
                         private val avgChannelsPerNode: Int,
                         private val blockchain: Blockchain) {
  import RandomGraphBuilder._

  def build(): Map[NodeID, NodeActor] = {
    val graph = new NetworkGraph()
    val router = new MinimalFeeRouter(MaximumRoutingFee)
    val params = NodeActor.Params(
      EclairDefaults.ReserveToFundingRatio,
      EclairDefaults.DustLimitSatoshis,
      EclairDefaults.MaxHTLCInFlight,
      EclairDefaults.MaxAcceptedHTLCs,
      EclairDefaults.HTLCMinimum,
      EclairDefaults.MinDepthBlocks,
      EclairDefaults.FinalExpiryDelta,
      EclairDefaults.ExpiryDelta,
      EclairDefaults.FeeBase,
      EclairDefaults.FeeProportionalMillionths
    )

    val nodes = for (_ <- 0 until numNodes) yield {
      val nodeID = Util.randomUUID()
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeID, blockchain)
      new NodeActor(nodeID, params, router, graphView, blockchainView)
    }
    Map(nodes.map(node => node.id -> node):_*)
  }

  /*
  private def createChannels(graph: NetworkGraph, numChannels: Int): Unit = {
    val nodeArray = graph.nodeIterator.toArray
    val randomBytes = Array.ofDim[Byte](32)

    Util.repeatUntilSuccess(numChannels) {
      val nodeA = nodeArray(Random.nextInt(nodeArray.length))
      val nodeB = nodeArray(Random.nextInt(nodeArray.length))
      if (nodeA != nodeB) {
        val capacity = 10000000000L // 0.1 BTC in mSAT

        val paramsA = nodeA.params.channelParams(capacity)
        val paramsB = nodeB.params.channelParams(capacity)

        val updateA = ChannelUpdate(
          timestamp = 0,
          disabled = false,
          expiryDelta = EclairDefaults.ExpiryDelta,
          htlcMinimum = paramsB.htlcMinimum,
          htlcMaximum = math.min(paramsB.maxHTLCInFlight, capacity),
          feeBase = EclairDefaults.FeeBase,
          feeProportionalMillionths = EclairDefaults.FeeProportionalMillionths,
        )
        val updateB = ChannelUpdate(
          timestamp = 0,
          disabled = false,
          expiryDelta = EclairDefaults.ExpiryDelta,
          htlcMinimum = paramsA.htlcMinimum,
          htlcMaximum = math.min(paramsA.maxHTLCInFlight, capacity),
          feeBase = EclairDefaults.FeeBase,
          feeProportionalMillionths = EclairDefaults.FeeProportionalMillionths,
        )

        val channel: Channel = new Channel(nodeA, nodeB, updateA, updateB)
        true
      } else {
        false
      }
    }
  }
    */
}

object RandomGraphBuilder {
  /**
    * This is the maximum that a node is willing to pay in routing fees. If the cost of a
    * transaction exceeds this amount, the node would prefer to send it on-chain anyway.
    *
    * Set to 0.0001 BTC.
    */
  val MaximumRoutingFee = 10000000
}
