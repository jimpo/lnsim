package edu.stanford.cs.lnsim

import java.util.UUID

import spray.json._
import spray.json.DefaultJsonProtocol._

object LNSJSONProtocol {
  val ChannelUpdateFormat : RootJsonFormat[ChannelUpdate] = jsonFormat7(ChannelUpdate)

  implicit object UUIDFormat extends RootJsonFormat[UUID] {
    def write(uuid: UUID): JsValue = uuid.toString.toJson
    def read(value: JsValue): UUID = ???
  }

  implicit object ChannelFormat extends RootJsonFormat[Channel] {
    def write(c: Channel) = JsObject(
      "id" -> JsString(c.id.toString),
      "nodeA" -> JsString(c.nodeA.id.toString),
      "nodeB" -> JsString(c.nodeB.id.toString))
    def read(value: JsValue): Channel = ???
  }

  implicit object NodeFormat extends RootJsonFormat[Node] {
    def write(n: Node) = JsObject("id" -> JsString(n.id.toString))
    def read(value: JsValue): Node = ???
  }

  implicit val PaymentInfoFormat = jsonFormat5(PaymentInfo)

  implicit val StartFormat = jsonFormat0(events.Start)
  implicit val NewBlockFormat = jsonFormat1(events.NewBlock)
  implicit val NewPaymentFormat = jsonFormat1(events.NewPayment)
  implicit val QueryNewPaymentFormat = jsonFormat1(events.QueryNewPayment)

  implicit object EventFormat extends JsonWriter[events.Base] {
    override def write(event: events.Base): JsValue = event match {
      case e @ events.Start() => StartFormat.write(e)
      case e @ events.NewBlock(_) => NewBlockFormat.write(e)
      case e @ events.NewPayment( _) => NewPaymentFormat.write(e)
      case events.ReceiveMessage(sender, recipient, message) =>
        JsObject(
          "sender" -> sender.toJson,
          "recipient" -> recipient.toJson,
          "message" -> message.getClass.toString.toJson
        )
      case e @ events.QueryNewPayment(_) => QueryNewPaymentFormat.write(e)
    }
  }
}
