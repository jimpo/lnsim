package edu.stanford.cs

import java.util.UUID

import edu.stanford.cs.lnsim.des.Timestamp
import edu.stanford.cs.lnsim.graph.{Channel, RouteConstraints}

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

  /** Parameters negotiated between nodes in a channel in the open_channel and accept_channel
    * messages in BOLT 2.
    */
  case class ChannelParams(requiredReserve: Value,
                           dustLimit: Value,
                           maxHTLCInFlight: Value,
                           maxAcceptedHTLCs: Int,
                           htlcMinimum: Value)

  case class HTLC(channel: Channel, desc: HTLC.Desc) {
    def channelID: ChannelID = channel.id
    def sender: NodeID = channel.source
    def recipient: NodeID = channel.target

    def id: HTLCID = desc.id
    def amount: Value = desc.amount
    def expiry: BlockNumber = desc.expiry
    def paymentID: PaymentID = desc.paymentID
  }

  object HTLC {
    /**
      * Description of an HTLC send within a channel.
      *
      * @param id The sequential ID of the HTLC within the channel. See BOLT 2, update_add_htlc.
      * @param amount The HTLC amount
      * @param expiry The HTLC expiry timestamp
      * @param paymentID The HTLC payment ID (payment hash in BOLT spec)
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
    * @param fallbackOnChain Whether this payment should be completed by opening a new channel if
    *                        it cannot be send off-chain.
    */
  case class PaymentInfo(sender: NodeID,
                         recipient: NodeID,
                         amount: Value,
                         finalExpiryDelta: BlockDelta,
                         paymentID: PaymentID,
                         fallbackOnChain: Boolean)

  /**
    * A complete routing packet that is sent through the circuit.
    *
    * @param hops Routing info for each hop forwards along the route.
    * @param finalHop Routing info on the final hop of the route.
    * @param backwardRoute Routing info for each hop backwards along the route.
    */
  case class ForwardRoutingPacket(hops: List[HTLC],
                                  finalHop: FinalHop,
                                  backwardRoute: BackwardRoutingPacket)

  case class BackwardRoutingPacket(hops: List[(Channel, HTLCID)])

  /**
    * A node announcement that is broadcast through network gossip. See BOLT 7.
    */
  case class NodeAnnouncement(nodeID: NodeID, timestamp: Timestamp)

  /**
    * Tracks the state of a pending payment from a node. This information is used to determine how
    * to retry the payment in case it fails.
    *
    * @param tries A count of the number of attempts to deliver this payment that already failed.
    */
  case class PendingPayment(info: PaymentInfo,
                            timestamp: Timestamp,
                            tries: Int,
                            route: List[ChannelID],
                            constraints: RouteConstraints)

  /**
    * This exception is thrown when a node implementation returns some invalid data.
    *
    * @param msg Message describing the error.
    */
  class MisbehavingNodeException(msg: String) extends Exception(msg)

  /** This exception is thrown if the simulation specification is invalid.
    */
  class InvalidSpecError(msg: String) extends Exception(msg)

  /**
    * This exception is thrown if an unrecoverable error or an error that violations simulation
    * assumptions occurs during HTLC processing.
    */
  class HTLCUpdateFailure(msg: String) extends Exception(msg)
}
