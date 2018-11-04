package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.LNSJSONProtocol.NodeFormat
import spray.json._

import scala.collection.mutable
import org.apache.logging.log4j.LogManager

class NetworkGraph {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty

  private val logger = LogManager.getLogger(classOf[NetworkGraph])

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator

  def addNode(node: Node): Unit = {
    nodes(node.id) = node
    logger.info(f"Node ${node.id}%s created: {}", node.toJson)
  }
}
