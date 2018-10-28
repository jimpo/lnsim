package edu.stanford.cs.lnsim

trait NodeBehavior {
  def route(paymentInfo: PaymentInfo): Array[HTLC]
}
