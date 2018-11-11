package edu.stanford.cs.lnsim.routing

import scala.collection.JavaConverters._
import edu.stanford.cs.lnsim._
import org.jgrapht.alg.shortestpath.DijkstraShortestPath

class MinimalFeeRouter(maxFee: Value) extends Router {

  /*
  /**
    * Find a route in the graph between localNodeId and targetNodeId
    *
    * @param g
    * @param localNodeId
    * @param targetNodeId
    * @param withEdges    those will be added before computing the route, and removed after so that g is left unchanged
    * @param withoutEdges those will be removed before computing the route, and added back after so that g is left unchanged
    * @return
    */
  def findRoute(g: DirectedWeightedPseudograph[PublicKey, DescEdge], localNodeId: PublicKey, targetNodeId: PublicKey, withEdges: Map[ChannelDesc, ChannelUpdate] = Map.empty, withoutEdges: Iterable[ChannelDesc] = Iterable.empty): Try[Seq[Hop]] = Try {
    if (localNodeId == targetNodeId) throw CannotRouteToSelf
    val workingGraph = if (withEdges.isEmpty && withoutEdges.isEmpty) {
      // no filtering, let's work on the base graph
      g
    } else {
      // slower but safer: we duplicate the graph and add/remove updates from the duplicated version
      val clonedGraph = g.clone().asInstanceOf[DirectedWeightedPseudograph[PublicKey, DescEdge]]
      withEdges.foreach { case (d, u) =>
        removeEdge(clonedGraph, d)
        addEdge(clonedGraph, d, u)
      }
      withoutEdges.foreach { d => removeEdge(clonedGraph, d) }
      clonedGraph
    }
    if (!workingGraph.containsVertex(localNodeId)) throw RouteNotFound
    if (!workingGraph.containsVertex(targetNodeId)) throw RouteNotFound
    val route_opt = Option(DijkstraShortestPath.findPathBetween(workingGraph, localNodeId, targetNodeId))
    route_opt match {
      case Some(path) => path.getEdgeList.map(edge => Hop(edge.desc.a, edge.desc.b, edge.u))
      case None => throw RouteNotFound
    }
  }
  */
  override def findPath(paymentInfo: PaymentInfo, graphView: NetworkGraphView): Iterator[ChannelWithDirection] = {
    val source = paymentInfo.sender.id
    val target = paymentInfo.recipient.id
    val weightedGraph = new FeeWeightedGraphDecorator(graphView.channelGraph, paymentInfo.amount)
    val path = DijkstraShortestPath.findPathBetween(weightedGraph, source, target)
    if (path != null && path.getWeight <= maxFee) {
      path.getEdgeList().iterator().asScala
    } else {
      Iterator.empty
    }
  }
}
