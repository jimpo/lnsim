package edu.stanford.cs.lnsim.graph

import edu.stanford.cs.lnsim.NodeID
import edu.stanford.cs.lnsim.routing.ChannelKey

/**
  * Defines restrictions on which routes a router may select. Route constraints are build up
  * through a node's history of payment failures and successes.
  */
class RouteConstraints private (private val ignoreNodes: Set[NodeID],
                                private val ignoreChannels: Set[ChannelKey]) {
  def this() = this(Set.empty, Set.empty)

  def +(other: RouteConstraints): RouteConstraints =
    new RouteConstraints(ignoreNodes ++ other.ignoreNodes, ignoreChannels ++ other.ignoreChannels)

  def banNode(nodeID: NodeID): RouteConstraints =
    new RouteConstraints(ignoreNodes + nodeID, ignoreChannels)

  def banChannel(channel: Channel): RouteConstraints =
    new RouteConstraints(ignoreNodes, ignoreChannels + ChannelKey(channel.id, channel.source))

  def allowNode(nodeID: NodeID): Boolean = !ignoreNodes.contains(nodeID)

  def allowChannel(channel: Channel): Boolean =
    allowNode(channel.source) &&
      allowNode(channel.target) &&
      !ignoreChannels.contains(ChannelKey(channel.id, channel.source))
}
