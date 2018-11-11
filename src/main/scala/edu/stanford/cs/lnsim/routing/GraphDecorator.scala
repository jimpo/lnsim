package edu.stanford.cs.lnsim.routing

import java.util
import java.util.function.Supplier

import org.jgrapht.{EdgeFactory, Graph, GraphType}

class GraphDecorator[V, E](protected val target: Graph[V, E]) extends Graph[V, E] {
  override def getAllEdges(sourceVertex: V, targetVertex: V): util.Set[E] =
    target.getAllEdges(sourceVertex, targetVertex)

  override def getEdge(sourceVertex: V, targetVertex: V): E =
    target.getEdge(sourceVertex, targetVertex)

  override def getEdgeFactory: EdgeFactory[V, E] = target.getEdgeFactory

  override def getVertexSupplier: Supplier[V] = target.getVertexSupplier

  override def getEdgeSupplier: Supplier[E] = target.getEdgeSupplier

  override def addEdge(sourceVertex: V, targetVertex: V): E =
    target.addEdge(sourceVertex, targetVertex)

  override def addEdge(sourceVertex: V, targetVertex: V, e: E): Boolean =
    target.addEdge(sourceVertex, targetVertex, e)

  override def addVertex(): V = target.addVertex()

  override def addVertex(v: V): Boolean = target.addVertex(v)

  override def containsEdge(sourceVertex: V, targetVertex: V): Boolean =
    target.containsEdge(sourceVertex, targetVertex)

  override def containsEdge(e: E): Boolean =
    target.containsEdge(e)

  override def containsVertex(v: V): Boolean =
    target.containsVertex(v)

  override def edgeSet(): util.Set[E] =
    target.edgeSet()

  override def degreeOf(vertex: V): Int =
    target.degreeOf(vertex)

  override def edgesOf(vertex: V): util.Set[E] =
    target.edgesOf(vertex)

  override def inDegreeOf(vertex: V): Int =
    target.inDegreeOf(vertex)

  override def incomingEdgesOf(vertex: V): util.Set[E] =
    target.incomingEdgesOf(vertex)

  override def outDegreeOf(vertex: V): Int =
    target.outDegreeOf(vertex)

  override def outgoingEdgesOf(vertex: V): util.Set[E] =
    target.outgoingEdgesOf(vertex)

  override def removeAllEdges(edges: util.Collection[_ <: E]): Boolean =
    target.removeAllEdges(edges)

  override def removeAllEdges(sourceVertex: V, targetVertex: V): util.Set[E] =
    target.removeAllEdges(sourceVertex, targetVertex)

  override def removeAllVertices(vertices: util.Collection[_ <: V]): Boolean =
    target.removeAllVertices(vertices)

  override def removeEdge(sourceVertex: V, targetVertex: V): E =
    target.removeEdge(sourceVertex, targetVertex)

  override def removeEdge(e: E): Boolean = target.removeEdge(e)

  override def removeVertex(v: V): Boolean = target.removeVertex(v)

  override def vertexSet(): util.Set[V] = target.vertexSet()

  override def getEdgeSource(e: E): V = target.getEdgeSource(e)

  override def getEdgeTarget(e: E): V = target.getEdgeTarget(e)

  override def getType: GraphType = target.getType

  override def getEdgeWeight(e: E): Double = target.getEdgeWeight(e)

  override def setEdgeWeight(e: E, weight: Double): Unit = target.setEdgeWeight(e, weight)
}
