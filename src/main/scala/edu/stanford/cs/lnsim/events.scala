package edu.stanford.cs.lnsim

package object events {
  sealed trait Base
  case object Start extends Base
  case class NewBlock(number: Int) extends Base
  case class ReceiveBlock(node: Node, number: Int) extends Base
  case class NewPayment(sender: Node, paymentInfo: PaymentInfo) extends Base
  case class ReceiveMessage(sender: Node, recipient: Node, message: Message) extends Base
  case class ReceiveHTLC(hopIndex: Int, route: RoutingPacket) extends Base
  case class FailHTLC(hopIndex: Int, route: RoutingPacket, error: RoutingError) extends Base
  case class FulfillHTLC(hopIndex: Int, route: RoutingPacket) extends Base
}

