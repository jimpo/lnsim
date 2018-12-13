package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.Timestamp

package object graph {
  /**
    * This structure contains the union of all information in BOLT 7 channel_announcement and
    * channel_update messages.
    */
  case class Channel(id: ChannelID,
                     source: NodeID,
                     target: NodeID,
                     lastUpdate: Timestamp,
                     disabled: Boolean,
                     expiryDelta: BlockDelta,
                     capacity: Value,
                     htlcMinimum: Value,
                     htlcMaximum: Value,
                     feeBase: Value,
                     feeProportionalMillionths: Long) {

    /**
      * Calculate the fee charged for an HTLC send through this channel.
      */
    def fee(amount: Value): Value = feeBase + amount * feeProportionalMillionths / 1000
  }
}
