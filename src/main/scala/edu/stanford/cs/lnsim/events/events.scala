package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.node.{NodeAction, NodeActor}

package object events {
  sealed trait Base

  /** Start is fired once when the simulation begins. The Environment handles this event by
    * initializing itself.
    */
  case class Start() extends Base

  /** NewBlock is fired when a new block is discovered. This is handled by registering the new
    * block with the blockchain, and handling any on-chain events that now confirmed, such as
    * channel openings.
    */
  case class NewBlock(number: Int) extends Base

  /** NewPayment is fired when a user initiates a new payment, originating externally to the
    * simulation environment.
    */
  case class NewPayment(paymentInfo: PaymentInfo) extends Base

  /** ReceiveMessage models delivery of a Lightning Network protocol message sent from one node to
    * another.
    */
  case class ReceiveMessage(sender: NodeActor, recipient: NodeActor, message: Message) extends Base
  case class QueryNewPayment() extends Base

  /** Node actions are scheduled by nodes that need to wait a certain amount of time before taking
    * some action such as retrying a failed payment. These should only be scheduled by the node that
    * the action is intended for -- nodes cannot schedule actions for each other.
    */
  case class ScheduledAction(node: NodeActor, action: NodeAction) extends Base

  /** OpenNewChannels signals to a node that it should open new channels with total capacity up to
    * a given amount. The node will decide for itself which channels to create, how many, and their
    * capacities automatically based on its local network graph information.
    */
  case class OpenChannels(node: NodeActor, budget: Value) extends Base
}
