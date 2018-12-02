package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.log.{StructuredLogger, StructuredLogging}
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._
import edu.stanford.cs.lnsim.node.{NodeAction, NodeActor, NodeContext}

import scala.util.Random

class Environment(private val nodes: Map[NodeID, NodeActor],
                  private var initialEvents: Seq[(TimeDelta, events.Base)],
                  private val blockchain: Blockchain) extends des.Environment with StructuredLogging {

  override type Event = events.Base

  def this(nodeSeq: Seq[NodeActor],
           initialEvents: Seq[(TimeDelta, events.Base)],
           blockchain: Blockchain) {
    this(nodeSeq.map(node => node.id -> node).toMap, initialEvents, blockchain)
  }

  def node(nodeID: NodeID): Option[NodeActor] = nodes.get(nodeID)

  override def initialEvent(): Event = {
    val event = events.Start(initialEvents)
    initialEvents = Seq()
    event
  }

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
      case events.Start(newEvents) =>
        scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(1))
        for ((delay, event) <- newEvents) {
          scheduleEvent(delay, event)
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
            implicit val ctx = new EnvNodeContext(timestamp, node, scheduleEvent)
            node.handleChannelOpenedOnChain(channelID)
          case Blockchain.ChannelClosed(channelID) => ???
          case Blockchain.ScheduledAction(nodeID, action) =>
            val node = nodes(nodeID)
            implicit val ctx = new EnvNodeContext(timestamp, node, scheduleEvent)
            node.handleAction(action)
        }
        scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(number + 1))

      case events.NewPayment(paymentInfo) =>
        val sender = paymentInfo.sender
        implicit val actions = new EnvNodeContext(timestamp, sender, scheduleEvent)
        sender.sendPayment(paymentInfo)

      case events.ReceiveMessage(sender, recipient, message) =>
        implicit val actions = new EnvNodeContext(timestamp, recipient, scheduleEvent)

        message match {
          case message @ OpenChannel(_, _, _, _) => recipient.handleOpenChannel(sender.id, message)
          case message @ AcceptChannel(_, _, _) => recipient.handleAcceptChannel(sender.id, message)
          case message @ FundingCreated(_, _) => recipient.handleFundingCreated(sender.id, message)
          case message @ UpdateAddHTLC(_) => recipient.handleUpdateAddHTLC(sender.id, message)
          case message @ UpdateFulfillHTLC(_) => recipient.handleUpdateFulfillHTLC(sender.id, message)
          case message @ UpdateFailHTLC( _, _, _) => recipient.handleUpdateFailHTLC(sender.id, message)
          case message @ Shutdown( _) => recipient.handleShutdown(sender.id, message)
        }

      case events.ScheduledAction(node, action) =>
        implicit val actions = new EnvNodeContext(timestamp, node, scheduleEvent)
        node.handleAction(action)

      case events.OpenChannels(node, budget) =>
        implicit val actions = new EnvNodeContext(timestamp, node, scheduleEvent)
        node.openNewChannels(budget)
    }
  }

  private def nodeReceiveTime(node: NodeActor): TimeDelta = Util.drawExponential(node.meanNetworkLatency)

  private class EnvNodeContext(private val initialTimestamp: Timestamp,
                               private val node: NodeActor,
                               private val scheduleEvent: (TimeDelta, Event) => Unit) extends NodeContext {
    private var timePassed: TimeDelta = 0

    override def timestamp: Timestamp = initialTimestamp + timePassed

    override def advanceTimestamp(time: TimeDelta): Unit = timePassed += time

    override def sendMessage(recipientID: NodeID, message: Message): Unit = {
      val recipient = nodes(recipientID)
      scheduleEvent(
        timePassed + nodeReceiveTime(recipient),
        events.ReceiveMessage(node, recipient, message)
      )
    }

    override def scheduleAction(delay: TimeDelta, action: NodeAction): Unit =
      scheduleEvent(timePassed + delay, events.ScheduledAction(node, action))
  }
}
