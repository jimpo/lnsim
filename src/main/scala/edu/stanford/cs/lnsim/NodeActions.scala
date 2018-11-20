package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}

trait NodeActions {
  def timestamp: Timestamp
  def retryPayment(delay: TimeDelta, payment: PendingPayment): Unit
  def sendMessage(delay: TimeDelta, recipient: NodeID, message: Message): Unit
}
