package edu.stanford.cs.lnsim

import java.util.UUID

case class ChannelUpdate(timestamp: Timestamp,
                         disabled: Boolean,
                         expiryDelta: TimeDelta,
                         htlcMinimum: Value,
                         htlcMaximum: Value,
                         feeBase: Value,
                         feeProportionalMillionths: Long)

object ChannelDirection extends Enumeration {
  type ChannelDirection = Value
  val AtoB, BtoA = Value
}
import ChannelDirection._

// TODO: Add features?
class Channel(val id: ChannelID,
              val nodeA: Node,
              val nodeB: Node,
              val update1: ChannelUpdate,
              val update2: ChannelUpdate) {

  def this(node1: NodeID, node2: NodeID, update1: ChannelUpdate, update2: ChannelUpdate) =
    this(UUID.randomUUID(), node1, node2, update1, update2)

  def sender(direction: ChannelDirection): Node = direction match {
    case AtoB => nodeA
    case BtoA => nodeB
  }

  def recipient(direction: ChannelDirection): Node = direction match {
    case AtoB => nodeB
    case BtoA => nodeA
  }

  override def toString: String = s"Channel($id, $nodeA <=> $nodeB)"
}
