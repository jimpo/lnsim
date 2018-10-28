package edu.stanford.cs.lnsim

package object events {
  sealed trait Base
  case object Start extends Base
  case class NewBlock(number: Int) extends Base
  case class ReceiveBlock(node: Node, number: Int) extends Base
  case class NewPayment(sender: Node, paymentInfo: PaymentInfo) extends Base
  case class ReceiveHTLC(node: Node, hopIndex: Int, route: ForwardRoutingPacket) extends Base
}

