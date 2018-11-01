package edu.stanford.cs

import java.util.UUID

import edu.stanford.cs.lnsim.ChannelDirection._
import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}

package object lnsim {
  type NodeID = UUID
  type ChannelID = UUID
  type PaymentID = UUID

  /***
    * Value is the base unit of currency supported. Per the BOLT spec, this is milli-satoshis for Bitcoin.
    */
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
                  channel: Channel,
                  direction: ChannelDirection,
                  amount: Value,
                  expiry: Timestamp,
                  paymentID: PaymentID) {
    def sender: Node = channel.sender(direction)
    def recipient: Node = channel.recipient(direction)
  }

  /**
    * Final hop data in a forward routing packet. See BOLT 4.
    *
    * @param amount This should equal the amount of the payment.
    * @param expiry This should match the required final expiry delta.
    * @param paymentIDKnown Whether the payment can be fulfilled by the recipient.
    */
  case class FinalHop(amount: Value,
                      expiry: Timestamp,
                      paymentIDKnown: Boolean)

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
    * A complete routing packet that is sent through the circuit.
    *
    * @param hops Routing info for each hop along the route.
    * @param finalHop Routing info on the final hop of the route.
    */
  case class RoutingPacket(hops: Array[HTLC], finalHop: FinalHop)

  /**
    * This exception is thrown when a node implementation returns some invalid data.
    *
    * @param msg Message describing the error.
    */
  class MisbehavingNodeException(msg: String) extends Exception(msg)
}
