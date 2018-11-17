package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.log.StructuredLogging
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

import scala.util.Random

class Environment(private val nodes: Map[NodeID, NodeActor],
                  private val blockchain: Blockchain) extends des.Environment with StructuredLogging {

  override type Event = events.Base

  override def initialEvent(): Event = events.Start()

  override def processEvent(event: Event,
                            timestamp: Timestamp,
                            scheduleEvent: (TimeDelta, Event) => Unit): Unit = event match {
    case events.Start() =>
      scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(0))
      for (node <- nodes.valuesIterator) {
        scheduleEvent(0, events.QueryNewPayment())
      }

    case events.NewBlock(number) =>
      for (notification <- blockchain.blockArrived()) notification match {
        case Blockchain.ChannelOpened(channelID, nodeID) =>
          val node = nodes(nodeID)
          implicit val sendMessage = createNetworkConnection(node, scheduleEvent) _
          node.handleChannelOpenedOnChain(channelID, timestamp)
        case Blockchain.ChannelClosed(channelID) =>
      }
      scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(number + 1))

    case events.NewPayment(paymentInfo) =>
      val sender = paymentInfo.sender
      implicit val sendMessage = createNetworkConnection(sender, scheduleEvent) _
      sender.sendPayment(paymentInfo)

    case events.ReceiveMessage(sender, recipient, message) =>
      implicit val sendMessage = createNetworkConnection(recipient, scheduleEvent) _

      message match {
        case message @ OpenChannel(_, _, _, _) => recipient.handleOpenChannel(sender.id, message)
        case message @ AcceptChannel(_, _, _) => recipient.handleAcceptChannel(sender.id, message)
        case message @ FundingCreated(_, _) => recipient.handleFundingCreated(sender.id, message)
        case message @ UpdateAddHTLC(_, _) => recipient.handleUpdateAddHTLC(sender.id, message)
        case message @ UpdateFulfillHTLC(_, _) => recipient.handleUpdateFulfillHTLC(sender.id, message)
        case message @ UpdateFailHTLC(_, _, _) => recipient.handleUpdateFailHTLC(sender.id, message)
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
  }

  private def nodeReceiveTime(node: NodeActor): TimeDelta = Util.drawExponential(node.meanNetworkLatency)

  private def createNetworkConnection(node: NodeActor, scheduleEvent: (TimeDelta, Event) => Unit)
                                     (delay: TimeDelta, msgRecipientID: NodeID, newMessage: Message): Unit = {
    val msgRecipient = nodes(msgRecipientID)
    scheduleEvent(
      delay + nodeReceiveTime(msgRecipient),
      events.ReceiveMessage(node, msgRecipient, newMessage)
    )
  }
}
