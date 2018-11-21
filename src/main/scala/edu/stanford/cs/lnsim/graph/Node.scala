package edu.stanford.cs.lnsim.graph

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.des.Timestamp

case class Node(id: NodeID, channels: Map[ChannelID, Channel] = Map.empty) {
  def updateChannel(channel: Channel): Node = {
    val lastUpdateTime: Timestamp = channels.get(channel.id).map(_.lastUpdate).getOrElse(-1)
    if (lastUpdateTime < channel.lastUpdate) {
      Node(id, channels + (channel.id -> channel))
    } else {
      this
    }
  }

  def removeChannel(channelID: ChannelID): Node = Node(id, channels - channelID)
}
