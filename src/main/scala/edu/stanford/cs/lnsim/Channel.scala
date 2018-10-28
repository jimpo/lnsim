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
class Channel(val id: ChannelID,
              val node1: NodeID,
              val node2: NodeID,
              val update1: ChannelUpdate,
              val update2: ChannelUpdate) {

  def this(node1: NodeID, node2: NodeID, update1: ChannelUpdate, update2: ChannelUpdate) =
    this(UUID.randomUUID(), node1, node2, update1, update2)

  override def toString: String = s"Channel($id, $node1 <=> $node2)"
}
