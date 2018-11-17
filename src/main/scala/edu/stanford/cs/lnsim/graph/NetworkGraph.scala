package edu.stanford.cs.lnsim.graph

import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.{ChannelID, NodeID}

import scala.collection.mutable

class NetworkGraph extends StructuredLogging {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty
  private val channels: mutable.Map[ChannelID, (NodeID, NodeID)] = mutable.HashMap.empty

  def updateChannel(channel: Channel): Unit = {
    val sourceID = channel.source
    val source = nodes.getOrElse(sourceID, Node(sourceID))
    nodes(sourceID) = source.updateChannel(channel)
    channels(channel.id) = (channel.source, channel.target)
  }

  def removeChannel(channelID: ChannelID): Unit = channels.remove(channelID) match {
    case Some((nodeA, nodeB)) =>
      for (node <- node(nodeA)) {
        nodes(nodeA) = node.removeChannel(channelID)
      }
      for (node <- node(nodeB)) {
        nodes(nodeB) = node.removeChannel(channelID)
      }

    case None =>
  }

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator
}
