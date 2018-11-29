package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.log.{StructuredLogger, StructuredLogging}
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

import scala.util.Random

class Environment(private val nodeSeq: Seq[NodeActor],
                  private val blockchain: Blockchain) extends des.Environment with StructuredLogging {

  override type Event = events.Base

  private val nodes: Map[NodeID, NodeActor] = nodeSeq.map(node => node.id -> node).toMap

  override def initialEvent(): Event = events.Start()

  override def processEvent(event: Event,
                            timestamp: Timestamp,
                            scheduleEvent: (TimeDelta, Event) => Unit): Unit = {
    StructuredLogger.setGlobal("time" -> timestamp.toJson)

    logger.debug(
      "message" -> "Processing event".toJson,
      "time" -> timestamp.toJson,
      "event" -> event.toJson
    )

    event match {
      case events.Start() =>
        scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(1))
        for (node <- nodes.valuesIterator) {
          scheduleEvent(0, events.QueryNewPayment())
        }

      case events.NewBlock(number) =>
        if (number != blockchain.blockNumber + 1) {
          throw new AssertionError(
            s"$event fired out of order, expected block ${blockchain.blockNumber + 1}"
          )
        }
        for (notification <- blockchain.blockArrived()) notification match {
          case Blockchain.ChannelOpened(channelID, nodeID) =>
            val node = nodes(nodeID)
            implicit val actions = new EnvNodeActions(timestamp, node, scheduleEvent)
            node.handleChannelOpenedOnChain(channelID)
          case Blockchain.ChannelClosed(channelID) =>
        }
        scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(number + 1))

      case events.NewPayment(paymentInfo) =>
        val sender = paymentInfo.sender
        implicit val actions = new EnvNodeActions(timestamp, sender, scheduleEvent)
        sender.sendPayment(paymentInfo)

      case events.ReceiveMessage(sender, recipient, message) =>
        implicit val actions = new EnvNodeActions(timestamp, recipient, scheduleEvent)

        message match {
          case message @ OpenChannel(_, _, _, _) => recipient.handleOpenChannel(sender.id, message)
          case message @ AcceptChannel(_, _, _) => recipient.handleAcceptChannel(sender.id, message)
          case message @ FundingCreated(_, _) => recipient.handleFundingCreated(sender.id, message)
          case message @ UpdateAddHTLC(_) => recipient.handleUpdateAddHTLC(sender.id, message)
          case message @ UpdateFulfillHTLC(_) => recipient.handleUpdateFulfillHTLC(sender.id, message)
          case message @ UpdateFailHTLC( _, _, _) => recipient.handleUpdateFailHTLC(sender.id, message)
        }

      case events.QueryNewPayment() =>

        val senderIdx = Random.nextInt(nodes.size)
        val sender = nodes.valuesIterator.drop(senderIdx).next()

        val recipientIdx = Random.nextInt(nodes.size)
        val recipient = nodes.valuesIterator.drop(recipientIdx).next()
        if (sender != recipient) {
          val paymentInfo = PaymentInfo(
            sender = sender,
            recipientID = recipient.id,
            amount = 1000,
            finalExpiryDelta = recipient.params.finalExpiryDelta,
            paymentID = Util.randomUUID()
          )

          logger.info(
            "msg" -> "Generating new payment".toJson,
            "paymentInfo" -> paymentInfo.toJson
          )
          scheduleEvent(0, events.NewPayment(paymentInfo))

          // Return the time delay until the node should next be queried to initiate new payments. This
          // is a lower bound on the next time a payment may be initiated by this node.
          //
          // The naive strategy implemented to send payments by a Poisson process with expected time
          // 10 minutes.
          val nextPaymentQuery = Util.drawExponential(10 * 60 * 1000)
          scheduleEvent(nextPaymentQuery, events.QueryNewPayment())
        }

      case events.RetryPayment(node, payment) =>
        implicit val actions = new EnvNodeActions(timestamp, node, scheduleEvent)
        node.executePayment(payment)

      case events.OpenChannels(node, budget) =>
        implicit val actions = new EnvNodeActions(timestamp, node, scheduleEvent)
        node.openNewChannels(budget)
    }
  }

  private def nodeReceiveTime(node: NodeActor): TimeDelta = Util.drawExponential(node.meanNetworkLatency)

  private class EnvNodeActions(val timestamp: Timestamp,
                               private val node: NodeActor,
                               private val scheduleEvent: (TimeDelta, Event) => Unit) extends NodeActions {
    override def sendMessage(delay: TimeDelta, recipientID: NodeID, message: Message): Unit = {
      val recipient = nodes(recipientID)
      scheduleEvent(
        delay + nodeReceiveTime(recipient),
        events.ReceiveMessage(node, recipient, message)
      )
    }

    override def retryPayment(delay: TimeDelta, payment: PendingPayment): Unit =
      scheduleEvent(delay, events.RetryPayment(node, payment))
  }
}
