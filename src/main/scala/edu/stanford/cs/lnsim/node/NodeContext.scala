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

  /** Schedule an action to be taken a given delay. */
  def scheduleAction(delay: TimeDelta, action: NodeAction): Unit

  /** Send a Lightning protocol message to another node on the network. */
  def sendMessage(recipient: NodeID, message: Message): Unit
}
