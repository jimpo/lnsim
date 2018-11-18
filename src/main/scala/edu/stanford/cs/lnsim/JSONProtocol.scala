package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.graph.{Channel, ChannelUpdate}
import spray.json._
import spray.json.DefaultJsonProtocol._

/**
  * Defines spray JSON writers for all lnsim objects.
  */
object JSONProtocol {
  val ChannelUpdateFormat : RootJsonFormat[ChannelUpdate] = jsonFormat7(ChannelUpdate)

  implicit object UUIDFormat extends JsonFormat[UUID] {
    override def write(uuid: UUID): JsValue = uuid.toString.toJson

    override def read(json: JsValue): PaymentID = ???
  }

  implicit object ChannelFormat extends JsonWriter[Channel] {
    override def write(c: Channel) = JsObject(
      "id" -> c.id.toJson,
      "source" -> c.source.toJson,
      "target" -> c.target.toJson,
    )
  }

  implicit object NodeFormat extends JsonWriter[NodeActor] {
    override def write(n: NodeActor) = JsObject("id" -> JsString(n.id.toString))
  }

  implicit object PaymentInfoFormat extends JsonFormat[PaymentInfo] {
    override def write(paymentInfo: PaymentInfo): JsValue = JsObject(
      "sender" -> paymentInfo.sender.id.toJson,
      "recipient" -> paymentInfo.recipientID.toJson,
      "amount" -> paymentInfo.amount.toJson,
      "finalExpiryDelta" -> paymentInfo.finalExpiryDelta.toJson,
      "paymentID" -> paymentInfo.paymentID.toJson,
    )

    override def read(json: JsValue): PaymentInfo = ???
  }

  implicit object PendingPaymentFormat extends JsonFormat[PendingPayment] {
    override def write(payment: PendingPayment): JsValue =
      Util.extendJsObject(
        payment.info.toJson.asJsObject,
        "createAt" -> payment.timestamp.toJson,
        "tries" -> payment.tries.toJson,
      )

    override def read(json: JsValue): PendingPayment = ???
  }

  implicit object RoutingErrorWriter extends JsonWriter[RoutingError] {
    override def write(error: RoutingError): JsValue = error.toString.toJson
  }

  implicit object MessageFormat extends JsonWriter[Message] {
    override def write(message: Message): JsValue = message match {
      case _ => message.toString.toJson
    }
  }

  implicit val StartFormat: RootJsonFormat[events.Start] =
    jsonFormat0(events.Start)
  implicit val NewBlockFormat: RootJsonFormat[events.NewBlock] =
    jsonFormat1(events.NewBlock)
  implicit val NewPaymentFormat: RootJsonFormat[events.NewPayment] =
    jsonFormat1(events.NewPayment)
  implicit val QueryNewPaymentFormat: RootJsonFormat[events.QueryNewPayment] =
    jsonFormat0(events.QueryNewPayment)

  implicit object EventFormat extends JsonWriter[events.Base] {
    override def write(event: events.Base): JsValue = event match {
      case e @ events.Start() => StartFormat.write(e)
      case e @ events.NewBlock(_) => NewBlockFormat.write(e)
      case e @ events.NewPayment( _) => NewPaymentFormat.write(e)
      case events.ReceiveMessage(sender, recipient, message) => JsObject(
        "name" -> "ReceiveMessage".toJson,
        "sender" -> sender.toJson,
        "recipient" -> recipient.toJson,
        "message" -> message.toJson,
      )
      case e @ events.QueryNewPayment() => QueryNewPaymentFormat.write(e)
      case e @ events.RetryPayment(_, payment) => Util.extendJsObject(
        payment.toJson.asJsObject,
        "name" -> "RetryPayment".toJson,
      )
    }
  }
}
