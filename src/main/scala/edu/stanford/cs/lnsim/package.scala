package edu.stanford.cs

import java.util.UUID

import edu.stanford.cs.lnsim.ChannelDirection._

package object lnsim {
  type NodeID = UUID
  type ChannelID = UUID
  type PaymentID = UUID
  type HTLCID = Int
  type BlockNumber = Int
  type BlockDelta = Int

  /***
    * Value is the base unit of currency supported. Per the BOLT spec, this is milli-satoshis for Bitcoin.
    */
  type Value = Long


  case class HTLC(channel: Channel,
                  direction: ChannelDirection,
                  desc: HTLC.Desc) {
    def sender: Node = channel.sender(direction)
    def recipient: Node = channel.recipient(direction)

    def id: Int = desc.id
    def amount: Value = desc.amount
    def expiry: BlockNumber = desc.expiry
    def paymentID: PaymentID = desc.paymentID
  }

  object HTLC {
    /**
      * Description of an HTLC send within a channel.
      *
      * @param id The sequential ID of the HTLC within the channel. See BOLT 2, update_add_htlc.
      * @param amount
      * @param expiry
      * @param paymentID
    */
    case class Desc(id: HTLCID,
                    amount: Value,
                    expiry: BlockNumber,
                    paymentID: PaymentID)
  }

  /**
    * Final hop data in a forward routing packet. See BOLT 4.
    *
    * @param amount This should equal the amount of the payment.
    * @param expiry This should match the required final expiry delta.
    * @param paymentIDKnown Whether the payment can be fulfilled by the recipient.
    */
  case class FinalHop(amount: Value,
                      expiry: BlockNumber,
                      paymentIDKnown: Boolean)

  /**
    * Description of a Lightning Network payment.
    *
    * @param sender The node sending the payment.
    * @param recipient The node receiving the payment.
    * @param amount The amount of the payment.
    * @param finalExpiryDelta The minimum expiry delta at the final hop.
    * @param paymentID The equivalent of a payment hash in the simulation environment.
    */
  case class PaymentInfo(sender: Node,
                         recipient: Node,
                         amount: Value,
                         finalExpiryDelta: BlockDelta,
                         paymentID: PaymentID)

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

  class HTLCUpdateFailure(msg: String) extends Exception(msg)
}
