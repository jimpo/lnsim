package edu.stanford.cs.lnsim

sealed trait Message
case class OpenChannel(id: ChannelID, capacity: Value, params: ChannelParams) extends Message
case class AcceptChannel(openMsg: OpenChannel, minimumDepth: BlockDelta, params: ChannelParams) extends Message
case class FundingCreated(channelID: ChannelID, acceptMsg: AcceptChannel) extends Message
case class UpdateAddHTLC(route: RoutingPacket, index: Int) extends Message
case class UpdateFulfillHTLC(route: RoutingPacket, index: Int) extends Message
case class UpdateFailHTLC(route: RoutingPacket, index: Int, error: RoutingError) extends Message
