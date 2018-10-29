package edu.stanford.cs.lnsim

import org.scalatest.{FlatSpec, Matchers}

class MockNodeBehavior extends NodeBehavior{}
class MockChannelUpdate extends ChannelUpdate(0,false,0,0,0,0,0)

class NetworkGraphSpec extends FlatSpec with Matchers {
  behavior of "A NetworkGraph"

  it should "add nodes" in {
    val node1 = new Node(new MockNodeBehavior)
    val node2 = new Node(new MockNodeBehavior)
    val node3 = new Node(new MockNodeBehavior)

    val graph = new NetworkGraph

    assert(graph.node(node1.id) == None)
    assert(graph.node(node2.id) == None)
    assert(graph.node(node3.id) == None)

    graph.addNode(node1)
    graph.addNode(node2)

    assert(graph.node(node1.id) == Some(node1))
    assert(graph.node(node2.id) == Some(node2))
    assert(graph.node(node3.id) == None)

    graph.addNode(node3)
    assert(graph.node(node3.id) == Some(node3))
  }

  it should "add channels" in {
    val node1 = new Node(new MockNodeBehavior)
    val node2 = new Node(new MockNodeBehavior)
    val channel = new Channel(node1.id, node2.id, new MockChannelUpdate, new MockChannelUpdate)

    val graph = new NetworkGraph

    graph.addNode(node1)
    graph.addNode(node2)

    graph.addChannel(node1, node2, channel)

    assert(graph.channel(channel.id).contains(channel))
  }
}
