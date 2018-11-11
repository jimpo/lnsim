package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{ChannelWithDirection, NodeAnnouncement, NodeID}
import org.jgrapht.graph.DirectedMultigraph

import scala.collection.mutable

class NetworkGraphView {
  val channelGraph: DirectedMultigraph[NodeID, ChannelWithDirection] =
    new DirectedMultigraph[NodeID, ChannelWithDirection](null, null, false)
  val nodeAnnouncements: mutable.Map[NodeID, NodeAnnouncement] = mutable.HashMap.empty
}
