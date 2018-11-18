package edu.stanford.cs.lnsim

package object events {
  sealed trait Base
  case class Start() extends Base
  case class NewBlock(number: Int) extends Base
  case class NewPayment(paymentInfo: PaymentInfo) extends Base
  case class ReceiveMessage(sender: NodeActor, recipient: NodeActor, message: Message) extends Base
  case class QueryNewPayment() extends Base
  case class RetryPayment(node: NodeActor, payment: PendingPayment) extends Base
}
