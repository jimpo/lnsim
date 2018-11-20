package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{ChannelID, NodeID}
import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraph, Node}

/**
  * This represents a specific node's view of the network graph, including the graph structure as
  * well as routing history.
  */
class NetworkGraphView(private val graph: NetworkGraph) {
  private var _constraints: RouteConstraints = new RouteConstraints()

  def node(id: NodeID): Option[Node] = graph.node(id)
  def nodeIterator: Iterator[Node] = graph.nodeIterator
  def updateChannel(channel: Channel): Unit = graph.updateChannel(channel)
  def removeChannel(channelID: ChannelID): Unit = graph.removeChannel(channelID)
  def banNode(id: NodeID): Unit = _constraints = constraints.banNode(id)
  def banChannel(channel: Channel): Unit = _constraints = constraints.banChannel(channel)

  def constraints: RouteConstraints = _constraints
}
