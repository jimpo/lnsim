package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{ChannelID, NodeID, Util}
import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraphView, RouteConstraints}
import org.jgrapht.alg.flow.DinicMFImpl
import org.jgrapht.graph.{Multigraph, WeightedMultigraph}

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

/** This attempts to detect which channels in the network are most important in connecting the
  * network. Informally, this means the channels that are used most often to route payments. The
  * estimator does not use non-public channel information like the history of payments on the
  * network or distribution of funds within a channel, so it can be seen as running from the point
  * of view of an arbitrary node on the network.
  *
  * The estimator works by partitioning the network randomly into two sets, one being the source
  * and one the sink, and solving for the min cut between the partitions. It runs some number of
  * trials and ranks the channels by the number of min cuts that they appear in.
  */
class CriticalEdgeEstimator(private val weighting: Channel => Double) {
  import CriticalEdgeEstimator._

  def analyze(graphView: NetworkGraphView, localConstraints: RouteConstraints): Analysis = {
    val jgraph = graphView.jgraph(localConstraints, weighting)
    val nodeCount = jgraph.vertexSet.size
    val edgeCount = jgraph.edgeSet.size

    implicit val ec: ExecutionContext = ExecutionContext.global
    val futures = for (_ <- 1 to NumTrials)
      yield Future { trial(deepCloneGraph(jgraph)) }
    val edgeCounts = Future.foldLeft(futures)(Map.empty[ChannelID, Int]) { (map, cut) =>
      cut.foldLeft(map) { (mapInner, channelID) =>
        mapInner + (channelID -> (mapInner.getOrElse(channelID, 0) + 1))
      }
    }
    val coreChannels = Await.result(edgeCounts, Duration.Inf).toList.sortBy(-_._2)
    Analysis(nodeCount, edgeCount, NumTrials, coreChannels)
  }

  private def trial(jgraph: Multigraph[NodeID, ChannelID]): Seq[ChannelID] = {
    // Create a source and sink and connected to nodes at random and determine the min cut of this
    // network.
    val source = Util.randomUUID()
    val sink = Util.randomUUID()
    jgraph.addVertex(source)
    jgraph.addVertex(sink)
    for (node <- jgraph.vertexSet.asScala if node != source && node != sink) {
      val channelID = Util.randomUUID()
      if (Random.nextBoolean()) {
        jgraph.addEdge(source, node, channelID)
      } else {
        jgraph.addEdge(node, sink, channelID)
      }
    }
    val alg = new DinicMFImpl[NodeID, ChannelID](jgraph)
    alg.calculateMinCut(source, sink)
    alg.getCutEdges.asScala.toSeq
  }

  private def deepCloneGraph(jgraph: Multigraph[NodeID, ChannelID])
  : Multigraph[NodeID, ChannelID] = {
    val newGraph = new WeightedMultigraph[NodeID, ChannelID](null, null)
    for (vertex <- jgraph.vertexSet.asScala) {
      newGraph.addVertex(vertex)
    }
    for (edge <- jgraph.edgeSet.asScala) {
      newGraph.addEdge(jgraph.getEdgeSource(edge), jgraph.getEdgeTarget(edge), edge)
      newGraph.setEdgeWeight(edge, jgraph.getEdgeWeight(edge))
    }
    newGraph
  }
}

object CriticalEdgeEstimator {
  private val NumTrials: Int = 20

  case class Analysis(nodeCount: Int,
                      edgeCount: Int,
                      trials: Int,
                      coreChannels: Seq[(ChannelID, Int)])
}
