package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.{Message, NodeID, PendingPayment}

/** The NodeContext is the NodeActor's interface to the simulation environment during event
  * handling.
  */
trait NodeContext {
  /** Get the current time in the simulation environment. */
  def timestamp: Timestamp

  /** Register that a given amount of time has passed. */
  def advanceTimestamp(time: TimeDelta): Unit

  /** Schedule a payment to be retried after a given delay. */
  def retryPayment(delay: TimeDelta, payment: PendingPayment): Unit

  /** Send a Lightning protocol message to another node on the network. */
  def sendMessage(recipient: NodeID, message: Message): Unit
}
