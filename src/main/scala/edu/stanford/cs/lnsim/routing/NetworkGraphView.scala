package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{ChannelID, NodeID}
import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraph, Node}

/**
  * This represents a specific node's view of the network graph, including the graph structure as
  * well as routing history.
  */
class NetworkGraphView(private val graph: NetworkGraph) {
  def node(id: NodeID): Option[Node] = graph.node(id)
  def nodeIterator: Iterator[Node] = graph.nodeIterator
  def updateChannel(channel: Channel): Unit = graph.updateChannel(channel)
  def removeChannel(channelID: ChannelID): Unit = graph.removeChannel(channelID)
}
