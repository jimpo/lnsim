package edu.stanford.cs.lnsim

sealed trait Message
case class UpdateAddHTLC(route: RoutingPacket, index: Int) extends Message
case class UpdateFulfillHTLC(route: RoutingPacket, index: Int) extends Message
case class UpdateFailHTLC(route: RoutingPacket, index: Int, error: RoutingError) extends Message
