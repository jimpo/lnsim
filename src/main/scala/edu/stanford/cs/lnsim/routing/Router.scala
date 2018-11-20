package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.graph.Channel
import edu.stanford.cs.lnsim.PaymentInfo

trait Router {
  /**
    * Returns a list of channels in order from sender to recipient.
    */
  def findPath(paymentInfo: PaymentInfo,
               graph: NetworkGraphView,
               constraints: RouteConstraints): List[Channel]
}
