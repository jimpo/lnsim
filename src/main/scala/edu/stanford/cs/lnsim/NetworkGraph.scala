package edu.stanford.cs.lnsim

import scala.collection.mutable

class NetworkGraph {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty
  private val channels: mutable.Map[ChannelID, Channel] = mutable.HashMap.empty
  private val nodesToChannels: mutable.Map[NodeID, List[ChannelID]] = mutable.HashMap.empty

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def channel(id: ChannelID): Option[Channel] = channels.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator

  def addNode(node: Node): Unit = {
    nodes(node.id) = node
    nodesToChannels(node.id) = List.empty
  }

  def addChannel(node1: Node, node2: Node, channel: Channel): Unit = {
    channels.put(channel.id, channel)
    nodesToChannels(node1.id) = channel.id :: nodesToChannels(node1.id)
    nodesToChannels(node2.id) = channel.id :: nodesToChannels(node2.id)
  }
}
