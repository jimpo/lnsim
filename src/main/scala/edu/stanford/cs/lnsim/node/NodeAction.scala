package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.Channel

sealed trait NodeAction
case class RetryPayment(payment: PendingPayment) extends NodeAction
case class ForwardHTLC(route: ForwardRoutingPacket) extends NodeAction
case class FailHTLC(route: BackwardRoutingPacket, error: RoutingError, channel: Option[Channel]) extends NodeAction
case class FulfillHTLC(route: BackwardRoutingPacket) extends NodeAction
case class OpenNewChannels(budget: Value) extends NodeAction

