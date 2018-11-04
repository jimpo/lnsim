package edu.stanford.cs.lnsim

package object events {
  sealed trait Base
  case class Start() extends Base
  case class NewBlock(number: Int) extends Base
  case class ReceiveBlock(node: Node, number: Int) extends Base
  case class NewPayment(sender: Node, paymentInfo: PaymentInfo) extends Base
  case class ReceiveMessage(sender: Node, recipient: Node, message: Message) extends Base
}

