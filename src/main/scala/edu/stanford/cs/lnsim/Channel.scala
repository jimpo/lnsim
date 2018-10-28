package edu.stanford.cs.lnsim

case class ChannelUpdate(timestamp: Timestamp,
                         disabled: Boolean,
                         expiryDelta: TimeDelta,
                         htlcMinimum: Value,
                         htlcMaximum: Value,
                         feeBase: Value,
                         feeProportionalMillionths: Long)

// TODO: Add features?
case class Channel(id: ChannelID, node1: NodeID, node2: NodeID, update1: ChannelUpdate, update2: ChannelUpdate)
