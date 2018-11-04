package edu.stanford.cs.lnsim

import scala.util.Random

/**
  * Extremely dumb graph builder that creates channels between random nodes
  * of a fixed capacity, where the initial channel balance is even.
  *
  * @param rand
  */
class RandomGraphBuilder(private val rand: Random) {
  def buildNodes(numNodes: Int): Map[NodeID, Node] = {
    val params = Node.Params(finalExpiryDelta = EclairDefaults.FinalExpiryDelta)
    val nodes = for (_ <- 0 until numNodes) yield new Node(params)
    Map(nodes.map((node: Node) => node.id -> node):_*)
  }

  def createChannels(nodes: Map[NodeID, Node], numChannels: Int): Unit = {
    val nodeArray = nodes.values.toArray
    val randomBytes = Array.ofDim[Byte](32)
    for (_ <- 0 until numChannels) {
      val nodeA = nodeArray(rand.nextInt(nodeArray.length))
      val nodeB = nodeArray(rand.nextInt(nodeArray.length))

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

      val updateA =  ChannelUpdate(
        timestamp = 0,
        disabled = false,
        expiryDelta = EclairDefaults.ExpiryDelta,
        htlcMinimum = paramsB.htlcMinimum,
        htlcMaximum = math.min(paramsB.maxHTLCInFlight, capacity),
        feeBase = EclairDefaults.FeeBase,
        feeProportionalMillionths = EclairDefaults.FeeProportionalMillionths,
      )
      val updateB =  ChannelUpdate(
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
    }
  }
}

