package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.LNSJSONProtocol.NodeFormat
import edu.stanford.cs.lnsim.log.StructuredLogging
import spray.json._

import scala.collection.mutable

class NetworkGraph extends StructuredLogging {
  private val nodes: mutable.Map[NodeID, Node] = mutable.HashMap.empty

  def node(id: NodeID): Option[Node] = nodes.get(id)
  def nodeIterator: Iterator[Node] = nodes.valuesIterator

  def addNode(node: Node): Unit = {
    nodes(node.id) = node
    logger.info(
      "msg" -> JsString("Node created"),
      "node" -> node.toJson
    )
  }
}
