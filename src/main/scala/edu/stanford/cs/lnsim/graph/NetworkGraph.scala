package edu.stanford.cs.lnsim.graph

import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.{ChannelID, NodeID}

import scala.collection.mutable
import spray.json._
import spray.json.DefaultJsonProtocol._
import edu.stanford.cs.lnsim.JSONProtocol._
import org.jgrapht.graph.{Multigraph, WeightedMultigraph}

/**
  * The global network graph. The Lightning gossip protocol defined in BOLT 7 is designed so all
  * nodes eventually synchronize a global view of the graph topology. For simplicity, the
  * simulation environment assumes that all nodes have a complete and up-to-date view of the graph
  * at all times. Vertices in the graph are node IDs and edges are channels with the latest channel
  * update parameters.
  */
class NetworkGraph extends StructuredLogging {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty
  private val channels: mutable.Map[ChannelID, (NodeID, NodeID)] = mutable.HashMap.empty

  def channels(channelID: ChannelID): Seq[Channel] = {
    channels.get(channelID).toSeq
      .flatMap(nodes => {
        val (node1, node2) = nodes
        Seq(node1, node2)
          .flatMap(node)
          .flatMap(_.channelsIterator.find(_.id == channelID))
      })
  }

  def updateChannel(channel: Channel): Unit = {
    logger.debug(
      "msg" -> "Updating channel".toJson,
      "channelID" -> channel.id.toJson,
      "source" -> channel.source.toJson,
      "target" -> channel.target.toJson,
    )
    val sourceID = channel.source
    val source = nodes.getOrElseUpdate(sourceID, new Node(sourceID))
    source.updateChannel(channel)

    // Store channel endpoints indexed by channel ID
    channels.get(channel.id) match {
      case Some((node1, node2)) =>
        if (!((node1 == channel.source && node2 == channel.target) ||
          (node2 == channel.source && node1 == channel.target))) {
          throw new Exception(
            s"""Cannot update channel ${channel.id} endpoints from ($node1, $node2) to
               |(${channel.source}, ${channel.target})""".stripMargin
          )
        }
      case None => channels.put(channel.id, (channel.source, channel.target))
    }
  }

  def removeChannel(channelID: ChannelID): Unit = channels.remove(channelID) match {
    case Some((nodeA, nodeB)) =>
      for (node <- node(nodeA)) {
        node.removeChannel(channelID)
      }
      for (node <- node(nodeB)) {
        node.removeChannel(channelID)
      }

    case None =>
  }

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator

  def jgraph(constraints: RouteConstraints, weighting: Channel => Double)
  : Multigraph[NodeID, ChannelID] = {
    val jgraph = new WeightedMultigraph[NodeID, ChannelID](null, null)
    for (node <- nodeIterator ;
         channel <- node.channelsIterator
         if !channel.disabled && constraints.allowChannel(channel)) {
      jgraph.addVertex(channel.source)
      jgraph.addVertex(channel.target)
      jgraph.addEdge(channel.source, channel.target, channel.id)
      jgraph.setEdgeWeight(channel.id, weighting(channel))
    }
    jgraph
  }
}
