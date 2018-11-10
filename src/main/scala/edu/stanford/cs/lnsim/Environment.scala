package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.util.Random

class Environment(private val rand: Random,
                  private val blockchain: Blockchain,
                  private val graphBuilder: RandomGraphBuilder) extends des.Environment {

  override type Event = events.Base

  private val networkGraph: NetworkGraph = graphBuilder.build()

  override def initialEvent(): Event = events.Start()

  override def processEvent(event: Event, scheduleEvent: (TimeDelta, Event) => Unit): Unit = event match {
    case events.Start() =>
      scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(0))
      for (node <- networkGraph.nodeIterator) {
        scheduleEvent(0, events.QueryNewPayment(node))
      }

    case events.NewBlock(number) =>
      blockchain.blockArrived()
      scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(number + 1))

      for (node <- networkGraph.nodeIterator) {
        scheduleEvent(nodeReceiveTime(node), events.ReceiveBlock(node, number))
      }

    case events.ReceiveBlock(_node, _number) =>

    case events.NewPayment(paymentInfo) =>
      val sender = paymentInfo.sender
      sender.router.findPath(paymentInfo, networkGraph) match {
        case Some(routingPacket) =>
          routingPacket.hops.headOption match {
            case Some(firstHop) =>
              if (firstHop.sender != sender) {
                throw new MisbehavingNodeException("First hop sender is not payment sender")
              }
              val receiveEvent = events.ReceiveMessage(
                sender,
                firstHop.recipient,
                UpdateAddHTLC(routingPacket, 0)
              )
              scheduleEvent(nodeReceiveTime(firstHop.recipient), receiveEvent)

            case None =>
          }
        case None =>
      }

    case events.ReceiveMessage(sender, recipient, message) =>
      implicit val sendMessage = (delay: TimeDelta, newRecipient: Node, newMessage: Message) =>
        scheduleEvent(
          delay + nodeReceiveTime(newRecipient),
          events.ReceiveMessage(recipient, newRecipient, newMessage)
        )

      message match {
        case message @ UpdateAddHTLC(_, _) => recipient.handleUpdateAddHTLC(sender, message, blockchain.blockNumber)
        case message @ UpdateFulfillHTLC(_, _) => recipient.handleUpdateFulfillHTLC(sender, message)
        case message @ UpdateFailHTLC(_, _, _) => recipient.handleUpdateFailHTLC(sender, message)
      }

    case events.QueryNewPayment(node) =>
      for (paymentInfo <- node.newPayments()) {
        scheduleEvent(0, events.NewPayment(paymentInfo))
      }
      scheduleEvent(node.nextPaymentQuery, events.QueryNewPayment(node))
  }

  private def nodeReceiveTime(node: Node): TimeDelta = Util.drawExponential(node.meanNetworkLatency, rand)
}
