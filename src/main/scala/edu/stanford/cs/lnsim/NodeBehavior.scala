package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

trait NodeBehavior {
  def route(paymentInfo: PaymentInfo): RoutingPacket
  def forwardHTLC(hop: HTLC, nextHop: HTLC): (TimeDelta, Option[RoutingError])
  def acceptHTLC(hop: HTLC, finalHop: FinalHop): (TimeDelta, Option[RoutingError])
}
