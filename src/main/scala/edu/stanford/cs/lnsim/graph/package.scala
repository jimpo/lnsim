package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.Timestamp

package object graph {
  case class ChannelUpdate(timestamp: Timestamp,
                           disabled: Boolean,
                           expiryDelta: BlockDelta,
                           htlcMinimum: Value,
                           htlcMaximum: Value,
                           feeBase: Value,
                           feeProportionalMillionths: Long)

  case class Channel(val id: ChannelID,
                     val source: NodeID,
                     val target: NodeID,
                     val lastUpdate: ChannelUpdate)
}
