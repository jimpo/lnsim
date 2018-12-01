package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.{ForwardRoutingPacket, PendingPayment}

sealed trait NodeAction
case class RetryPayment(payment: PendingPayment) extends NodeAction
case class FailForwardHTLC(route: ForwardRoutingPacket) extends NodeAction