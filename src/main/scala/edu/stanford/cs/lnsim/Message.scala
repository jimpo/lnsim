package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.graph.Channel

/** Message is a superclass of all Lightning Protocol network messages.
  */
sealed trait Message
case class OpenChannel(id: ChannelID, capacity: Value, pushAmount: Value, params: ChannelParams) extends Message
case class AcceptChannel(openMsg: OpenChannel, minimumDepth: BlockDelta, params: ChannelParams) extends Message
case class FundingCreated(channelID: ChannelID, acceptMsg: AcceptChannel) extends Message
case class UpdateAddHTLC(route: ForwardRoutingPacket) extends Message
case class UpdateFulfillHTLC(route: BackwardRoutingPacket) extends Message
case class UpdateFailHTLC(route: BackwardRoutingPacket, error: RoutingError, channel: Option[Channel]) extends Message
case class Shutdown(id: ChannelID) extends Message
