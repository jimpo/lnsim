package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.log.StructuredLogging
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._
import edu.stanford.cs.lnsim.des.Timestamp

class LoggingOutput extends ObservableOutput with StructuredLogging {
  override def openChannel(channelID: ChannelID,
                           initiatingNode: NodeID,
                           receivingNode: NodeID,
                           capacity: Value,
                           fee: Value,
                           paymentID: Option[PaymentID]): Unit = {
    logger.info(
      "msg" -> "Opening new channel".toJson,
      "channelID" -> channelID.toJson,
      "initiatingNode" -> initiatingNode.toJson,
      "receivingNode" -> receivingNode.toJson,
      "capacity" -> capacity.toJson,
      "fee" -> fee.toJson,
      "paymentID" -> paymentID.toJson,
    )
  }

  override def paymentCompleted(pendingPayment: PendingPayment, timestamp: Timestamp): Unit = {
    logger.info(
      "msg" -> "Payment completed".toJson,
      "paymentID" -> pendingPayment.info.paymentID.toJson,
      "onChain" -> pendingPayment.route.isEmpty.toJson,
      "amount" -> pendingPayment.info.amount.toJson,
      "tries" -> pendingPayment.tries.toJson,
      "route" -> pendingPayment.route.toJson,
      "totalTime" -> (timestamp - pendingPayment.timestamp).toJson,
    )
  }
}
