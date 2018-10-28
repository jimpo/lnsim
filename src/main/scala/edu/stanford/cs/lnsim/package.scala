package edu.stanford.cs

import java.util.UUID

import edu.stanford.cs.lnsim.ChannelDirection._

package object lnsim {
  type NodeID = UUID
  type ChannelID = UUID
  type PaymentID = UUID

  type Timestamp = Long
  type TimeDelta = Long
  type Value = Long

  /**
    * Description of an HTLC send within a channel.
    *
    * @param id The sequential ID of the HTLC within the channel. See BOLT 2, update_add_htlc.
    * @param channel
    * @param direction
    * @param amount
    * @param expiry
    * @param paymentID
    */
  case class HTLC(id: Int,
                  channel: Option[Channel],
                  direction: ChannelDirection,
                  amount: Value,
                  expiry: Timestamp,
                  paymentID: PaymentID)

  /**
    * Description of a Lightning Network payment.
    *
    * @param recipient The node receiving the payment.
    * @param amount The amount of the payment.
    * @param finalExpiryDelta The minimum expiry delta at the final hop.
    * @param paymentID The equivalent of a payment hash in the simulation environment.
    */
  case class PaymentInfo(recipient: Node, amount: Value, finalExpiryDelta: TimeDelta, paymentID: PaymentID)

  /**
    * A routing packet that is sent forward through the circuit from sender to recipient.
    *
    * @param hops Routing info for each hop along the route.
    * @param valid Whether the payment preimage is known by the final recipient.
    */
  case class ForwardRoutingPacket(hops: Array[HTLC], valid: Boolean)

  /**
    * A routing packet that is sent backward through the circuit from recipient to sender.
    *
    * @param hops Routing info for each hop along the route.
    * @param error If the payment failed, the index of the hop where it failed and an error.
    */
  case class BackwardRoutingPacket(hops: Array[HTLC], error: Option[(Int, RoutingError)])

  /**
    * This exception is thrown when a node implementation returns some invalid data.
    *
    * @param msg Message describing the error.
    */
  class MisbehavingNodeException(msg: String) extends Exception(msg)
}
