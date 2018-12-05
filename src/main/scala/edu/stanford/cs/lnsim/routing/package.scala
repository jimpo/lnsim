package edu.stanford.cs.lnsim

package object routing {
  case class ChannelKey(channelID: ChannelID, sourceID: NodeID) {
    override def hashCode(): BlockDelta = channelID.hashCode() ^ sourceID.hashCode()
  }
}
