package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraph}
import org.scalatest.{FlatSpec, Matchers}

class NetworkGraphSpec extends FlatSpec with Matchers {
  behavior of "A NetworkGraph"

  it should "add and remove channels" in {
    val nodeID1 = Util.randomUUID()
    val nodeID2 = Util.randomUUID()
    val nodeID3 = Util.randomUUID()
    val nodeID4 = Util.randomUUID()

    val channelID1 = Util.randomUUID()
    val channelID2 = Util.randomUUID()
    val channelID3 = Util.randomUUID()

    // Channel 1 is between node 1 and node 2
    val channel1AtoB = Channel(
      id = channelID1,
      source = nodeID1,
      target = nodeID2,
      lastUpdate = 0,
      disabled = false,
      expiryDelta = EclairDefaults.ExpiryDelta,
      htlcMinimum = EclairDefaults.HTLCMinimum,
      htlcMaximum = EclairDefaults.MaxHTLCInFlight,
      feeBase = EclairDefaults.FeeBase,
      feeProportionalMillionths = EclairDefaults.FeeProportionalMillionths
    )
    val channel1BtoA = channel1AtoB.copy(
      source = channel1AtoB.target,
      target = channel1AtoB.source,
      lastUpdate = 1,
    )

    // Channel 2 has the same endpoints as channel 1
    val channel2AtoB = channel1AtoB.copy(
      id = channelID2,
    )
    val channel2BtoA = channel2AtoB.copy(
      source = channel1AtoB.target,
      target = channel1AtoB.source,
    )

    // Channel 3 is between node 1 and node 3
    val channel3AtoB = channel1AtoB.copy(
      id = channelID3,
      source = nodeID1,
      target = nodeID3,
    )
    val channel3BtoA = channel3AtoB.copy(
      source = channel3AtoB.target,
      target = channel3AtoB.source,
    )

    val graph = new NetworkGraph()
    graph.updateChannel(channel1AtoB)
    graph.updateChannel(channel1BtoA)
    graph.updateChannel(channel2AtoB)
    graph.updateChannel(channel2BtoA)
    graph.updateChannel(channel3AtoB)
    graph.updateChannel(channel3BtoA)

    assert(graph.node(nodeID1).isDefined)
    var node1 = graph.node(nodeID1).get
    assert(node1.channels.get(channelID1) == Some(channel1AtoB))
    assert(node1.channels.get(channelID2) == Some(channel2AtoB))
    assert(node1.channels.get(channelID3) == Some(channel3AtoB))

    assert(graph.node(nodeID2).isDefined)
    var node2 = graph.node(nodeID2).get
    assert(node2.channels.get(channelID1) == Some(channel1BtoA))
    assert(node2.channels.get(channelID2) == Some(channel2BtoA))
    assert(node2.channels.get(channelID3) == None)

    assert(graph.node(nodeID3).isDefined)
    var node3 = graph.node(nodeID3).get
    assert(node3.channels.get(channelID1) == None)
    assert(node3.channels.get(channelID2) == None)
    assert(node3.channels.get(channelID3) == Some(channel3BtoA))

    assert(graph.node(nodeID4).isEmpty)

    graph.removeChannel(channelID1)
    assert(graph.node(nodeID1).get.channels.size == 2)
    assert(graph.node(nodeID2).get.channels.size == 1)
    assert(graph.node(nodeID3).get.channels.size == 1)

    graph.removeChannel(channelID3)
    assert(graph.node(nodeID1).get.channels.size == 1)
    assert(graph.node(nodeID2).get.channels.size == 1)
    assert(graph.node(nodeID3).get.channels.size == 0)
  }

  it should "update channels with highest last updated timestamps" in {
    val nodeID1 = Util.randomUUID()
    val nodeID2 = Util.randomUUID()

    val channelID1 = Util.randomUUID()

    // Channel 1 is between node 1 and node 2
    val oldChannelUpdate = Channel(
      id = channelID1,
      source = nodeID1,
      target = nodeID2,
      lastUpdate = 0,
      disabled = false,
      expiryDelta = EclairDefaults.ExpiryDelta,
      htlcMinimum = EclairDefaults.HTLCMinimum,
      htlcMaximum = EclairDefaults.MaxHTLCInFlight,
      feeBase = EclairDefaults.FeeBase,
      feeProportionalMillionths = EclairDefaults.FeeProportionalMillionths
    )
    val newChannelUpdate = oldChannelUpdate.copy(
      lastUpdate = oldChannelUpdate.lastUpdate + 1
    )

    val graph = new NetworkGraph()

    graph.updateChannel(oldChannelUpdate)
    assert(graph.node(nodeID1).flatMap(_.channels.get(channelID1)) == Some(oldChannelUpdate))

    graph.updateChannel(newChannelUpdate)
    assert(graph.node(nodeID1).flatMap(_.channels.get(channelID1)) == Some(newChannelUpdate))

    graph.updateChannel(oldChannelUpdate)
    assert(graph.node(nodeID1).flatMap(_.channels.get(channelID1)) == Some(newChannelUpdate))
  }
}
