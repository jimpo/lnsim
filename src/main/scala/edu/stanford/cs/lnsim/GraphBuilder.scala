package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.graph.NetworkGraph
import edu.stanford.cs.lnsim.node.{BlockchainView, NodeActor}
import edu.stanford.cs.lnsim.routing.{MinimalFeeRouter, NetworkGraphView}

class GraphBuilder(private val numNodes: Int,
                   private val avgChannelsPerNode: Int,
                   private val blockchain: Blockchain) {
  import GraphBuilder._

  def build(): Seq[NodeActor] = {
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
      EclairDefaults.FeeProportionalMillionths,
      LndDefaults.AutoPilotMinChannelSize,
      LndDefaults.AutoPilotMinChannelSize,
      FundingTransactionWeight,
    )

    for (_ <- 0 until numNodes) yield {
      val nodeID = Util.randomUUID()
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeID, blockchain)
      new NodeActor(nodeID, params, router, graphView, blockchainView)
    }
  }
}

object GraphBuilder {
  /** This is the maximum that a node is willing to pay in routing fees. If the cost of a
    * transaction exceeds this amount, the node would prefer to send it on-chain anyway.
    *
    * Set to 0.0001 BTC.
    */
  val MaximumRoutingFee: Value = 10000000

  /** Assume the weight of the funding transaction in BOLT 3: Appendix B test vectors, which is 928.
    */
  val FundingTransactionWeight: Int = 928
}
