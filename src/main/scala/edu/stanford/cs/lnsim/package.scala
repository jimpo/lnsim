package edu.stanford.cs

import java.util.UUID

package object lnsim {
  type NodeID = UUID
  type ChannelID = UUID
  type PaymentID = UUID

  type Timestamp = Long
  type TimeDelta = Long
  type Value = Long

  // channelID -1 indicates the last hop in a route
  case class HTLC(id: Int, channelID: ChannelID, amount: Value, expiry: Timestamp, paymentID: PaymentID)

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
    * @param error If the payment failed, the index of the hop where it failed.
    */
  case class BackwardRoutingPacket(hops: Array[HTLC], error: Option[Int])
}
