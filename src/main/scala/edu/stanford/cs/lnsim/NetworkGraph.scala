package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.LNSJSONProtocol.{ChannelFormat, NodeFormat}
import spray.json._

import scala.collection.mutable
import org.apache.logging.log4j.LogManager

class NetworkGraph {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty
  private val channels: mutable.Map[ChannelID, Channel] = mutable.HashMap.empty
  private val nodesToChannels: mutable.Map[NodeID, List[ChannelID]] = mutable.HashMap.empty

  private val logger = LogManager.getLogger(classOf[NetworkGraph])

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def channel(id: ChannelID): Option[Channel] = channels.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator

  def channelPeer(channelID: ChannelID, nodeID: NodeID): Option[Node] =
    channels.get(channelID).flatMap(channel =>
      if (channel.nodeA.id == nodeID) Some(channel.nodeA)
      else if (channel.nodeB.id == nodeID) Some(channel.nodeB)
      else None
    )

  def addNode(node: Node): Unit = {
    nodes(node.id) = node
    nodesToChannels(node.id) = List.empty

    logger.info(f"Node ${node.id}%s created: {}", node.toJson)
  }

  def addChannel(node1: Node, node2: Node, channel: Channel): Unit = {
    channels.put(channel.id, channel)

    logger.info(f"Channel ${channel.id}%s created with parties ${node1.id}%s and ${node2.id}%s: {}", channel.toJson)

    nodesToChannels(node1.id) = channel.id :: nodesToChannels(node1.id)
    nodesToChannels(node2.id) = channel.id :: nodesToChannels(node2.id)
  }
}
