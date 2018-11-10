package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.routing.DummyRouter

import scala.util.Random

/**
  * Extremely dumb graph builder that creates channels between random nodes
  * of a fixed capacity, where the initial channel balance is even.
  *
  * @param rand
  */
class RandomGraphBuilder(private val rand: Random,
                         private val numNodes: Int,
                         private val avgChannelsPerNode: Int) {
  def build(): NetworkGraph = {
    val graph = new NetworkGraph()
    buildNodes(graph, numNodes)
    createChannels(graph, numNodes * avgChannelsPerNode)
    graph
  }

  private def buildNodes(graph: NetworkGraph, numNodes: Int): Unit = {
    val router = new DummyRouter()
    val params = Node.Params(finalExpiryDelta = EclairDefaults.FinalExpiryDelta)
    for (_ <- 0 until numNodes) {
      graph.addNode(new Node(params, router))
    }
  }

  private def createChannels(graph: NetworkGraph, numChannels: Int): Unit = {
    val nodeArray = graph.nodeIterator.toArray
    val randomBytes = Array.ofDim[Byte](32)

    Util.repeatUntilSuccess(numChannels) {
      val nodeA = nodeArray(rand.nextInt(nodeArray.length))
      val nodeB = nodeArray(rand.nextInt(nodeArray.length))
      if (nodeA != nodeB) {
        val capacity = 10000000000L // 0.1 BTC in mSAT

        val paramsA = ChannelParams(
          requiredReserve = (capacity * EclairDefaults.ReserveToFundingRatio).toLong,
          dustLimit = EclairDefaults.DustLimitSatoshis,
          maxHTLCInFlight = EclairDefaults.MaxHTLCInFlight,
          maxAcceptedHTLCs = EclairDefaults.MaxAcceptedHTLCs,
          htlcMinimum = EclairDefaults.HTLCMinimum,
        )
        val paramsB = ChannelParams(
          requiredReserve = (capacity * EclairDefaults.ReserveToFundingRatio).toLong,
          dustLimit = EclairDefaults.DustLimitSatoshis,
          maxHTLCInFlight = EclairDefaults.MaxHTLCInFlight,
          maxAcceptedHTLCs = EclairDefaults.MaxAcceptedHTLCs,
          htlcMinimum = EclairDefaults.HTLCMinimum,
        )

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
        nodeA.channelOpen(channel, paramsA, paramsB)
        nodeB.channelOpen(channel, paramsB, paramsA)
        true
      } else {
        false
      }
    }
  }
}
