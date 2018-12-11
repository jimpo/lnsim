package edu.stanford.cs.lnsim

package object routing {
  case class ChannelKey(channelID: ChannelID, sourceID: NodeID) {
    override def hashCode(): Int = channelID.hashCode() ^ sourceID.hashCode()
  }

  class RoutingException(msg: String) extends Exception(msg)
}
