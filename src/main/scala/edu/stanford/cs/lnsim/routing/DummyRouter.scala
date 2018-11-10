package edu.stanford.cs.lnsim.routing
import edu.stanford.cs.lnsim.{NetworkGraph, PaymentInfo, RoutingPacket}

class DummyRouter extends Router {
  override def findPath(paymentInfo: PaymentInfo, networkGraph: NetworkGraph): Option[RoutingPacket] = None
}

