package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.{ChannelWithDirection, PaymentInfo}

trait Router {
  /**
    * Returns an iterator of channels in order from sender to recipient.
    */
  def findPath(paymentInfo: PaymentInfo, networkGraph: NetworkGraphView): Iterator[ChannelWithDirection]
}
