package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.Timestamp

trait ObservableOutput {
  def openChannel(channelID: ChannelID,
                  initiatingNode: NodeID,
                  receivingNode: NodeID,
                  capacity: Value,
                  fee: Value,
                  paymentID: Option[PaymentID]): Unit

  def paymentCompleted(pendingPayment: PendingPayment, timestamp: Timestamp): Unit
}
