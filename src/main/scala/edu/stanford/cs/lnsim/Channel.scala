package edu.stanford.cs.lnsim

import java.util.UUID

case class ChannelUpdate(timestamp: Timestamp,
                         disabled: Boolean,
                         expiryDelta: TimeDelta,
                         htlcMinimum: Value,
                         htlcMaximum: Value,
                         feeBase: Value,
                         feeProportionalMillionths: Long)

// TODO: Add features?
class Channel(node1: NodeID, node2: NodeID, update1: ChannelUpdate, update2: ChannelUpdate) {
  val id : UUID = UUID.randomUUID()
}
