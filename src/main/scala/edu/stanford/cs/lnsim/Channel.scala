package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.des.Timestamp

case class ChannelUpdate(timestamp: Timestamp,
                         disabled: Boolean,
                         expiryDelta: BlockDelta,
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
              val updateA: ChannelUpdate,
              val updateB: ChannelUpdate) {

  // TODO: Use implicit Random generator
  def this(nodeA: Node, nodeB: Node, updateA: ChannelUpdate, updateB: ChannelUpdate) =
    this(UUID.randomUUID(), nodeA, nodeB, updateA, updateB)

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
