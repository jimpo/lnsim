package edu.stanford.cs.lnsim

import java.util.UUID

import edu.stanford.cs.lnsim.graph.Channel
import edu.stanford.cs.lnsim.node.NodeActor
import edu.stanford.cs.lnsim.spec.{ChannelBudgetSpec, NodeSpec, SimulationSpec, TransactionSpec}
import spray.json._
import spray.json.DefaultJsonProtocol._

/**
  * Defines spray JSON writers for all lnsim objects.
  */
object JSONProtocol {
  implicit object UUIDFormat extends JsonFormat[UUID] {
    override def write(uuid: UUID): JsValue = uuid.toString.toJson

    override def read(json: JsValue): UUID = json match {
      case JsString(s) => UUID.fromString(s)
      case _ => throw new DeserializationException("UUID expected")
    }
  }

  val ChannelUpdateFormat : RootJsonFormat[Channel] = jsonFormat11(Channel)

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

  implicit val NewBlockFormat: RootJsonFormat[events.NewBlock] =
    jsonFormat1(events.NewBlock)
  implicit val NewPaymentFormat: RootJsonFormat[events.NewPayment] =
    jsonFormat1(events.NewPayment)

  implicit object EventFormat extends JsonWriter[events.Base] {
    override def write(event: events.Base): JsValue = event match {
      case e @ events.Start(_) => JsObject(
        "name" -> "ReceiveMessage".toJson,
      )
      case e @ events.NewBlock(_) => NewBlockFormat.write(e)
      case e @ events.NewPayment( _) => NewPaymentFormat.write(e)
      case events.ReceiveMessage(sender, recipient, message) => JsObject(
        "name" -> "ReceiveMessage".toJson,
        "sender" -> sender.toJson,
        "recipient" -> recipient.toJson,
        "message" -> message.toJson,
      )
      case e @ events.ScheduledAction(node, action) => JsObject(
        "name" -> "ScheduledAction".toJson,
        "node" -> node.id.toJson,
        "action" -> action.getClass.getSimpleName.toJson,
      )
      case e @ events.OpenChannels(node, budget) => JsObject(
        "name" -> "OpenChannels".toJson,
        "node" -> node.id.toJson,
        "budget" -> budget.toJson,
      )
      case e @ events.BootstrapEnd() => JsObject("name" -> "BoostrapEnd".toJson)
    }
  }

  implicit object NodeSpecFormat extends JsonReader[NodeSpec] {
    override def read(json: JsValue): NodeSpec = {
      json.asJsObject.getFields("id") match {
        case Seq(id) =>
          NodeSpec(id = id.convertTo[NodeID])
        case _ => throw new DeserializationException("TransactionSpec expected")
      }
    }
  }

  implicit object TransactionSpecFormat extends JsonReader[TransactionSpec] {
    override def read(json: JsValue): TransactionSpec = {
      json.asJsObject.getFields("Timestamp", "Sender", "Recipient", "Amount", "ID") match {
        case Seq(JsNumber(timestamp), sender, recipient, JsNumber(amount), paymentID) =>
          TransactionSpec(
            timestamp = timestamp.toLongExact,
            sender = sender.convertTo[NodeID],
            recipient = recipient.convertTo[NodeID],
            amount = amount.toLong,
            paymentID = paymentID.convertTo[NodeID],
          )
        case _ => throw new DeserializationException(s"TransactionSpec expected, got $json")
      }
    }
  }

  implicit object ChannelBudgetSpecFormat extends JsonReader[ChannelBudgetSpec] {
    override def read(json: JsValue): ChannelBudgetSpec = {
      json.asJsObject.getFields("Opener", "Time", "Amount") match {
        case Seq(node, JsNumber(timestamp), JsNumber(amount)) =>
          ChannelBudgetSpec(
            node = node.convertTo[NodeID],
            timestamp = timestamp.toLongExact,
            amount = amount.toLongExact,
          )
        case _ => throw new DeserializationException(s"TransactionSpec expected, got $json")
      }
    }
  }

  implicit object SimulationSpecFormat extends JsonReader[SimulationSpec] {
    override def read(json: JsValue): SimulationSpec = {
      json.asJsObject.getFields("NodeIDs", "Transactions", "ChannelBudgets", "BootstrapEnd", "SimulationEnd") match {
        case Seq(JsArray(nodeIDs), JsArray(transactions), JsArray(channelBudgets), JsNumber(startTime), JsNumber(endTime)) =>
          SimulationSpec(
            nodes = nodeIDs.map(_.convertTo[NodeID]).map(NodeSpec(_)).toList,
            transactions = transactions.map(_.convertTo[TransactionSpec]).toList,
            channelBudgets = channelBudgets.map(_.convertTo[ChannelBudgetSpec]).toList,
            startTime = startTime.toLongExact,
            endTime = endTime.toLongExact,
          )
        case _ => throw new DeserializationException("SimulationSpec expected")
      }
    }
  }
}
