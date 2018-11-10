package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{NetworkGraph, PaymentInfo, RoutingPacket}

trait Router {
  def findPath(paymentInfo: PaymentInfo, networkGraph: NetworkGraph): Option[RoutingPacket]
}
