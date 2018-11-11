package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

class Environment(private val blockchain: Blockchain,
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
      for (notification <- blockchain.blockArrived()) notification match {
        case Blockchain.ChannelOpened(channelID, nodeID) =>
          val node = networkGraph.node(nodeID).get
          implicit val sendMessage = createNetworkConnection(node, scheduleEvent) _
          node.handleChannelOpenedOnChain(channelID)
        case Blockchain.ChannelClosed(channelID) =>
      }
      scheduleEvent(blockchain.nextBlockTime(), events.NewBlock(number + 1))

    case events.NewPayment(paymentInfo) =>
      val sender = paymentInfo.sender
      implicit val sendMessage = (delay: TimeDelta, msgRecipient: Node, newMessage: Message) =>
        scheduleEvent(
          delay + nodeReceiveTime(msgRecipient),
          events.ReceiveMessage(sender, msgRecipient, newMessage)
        )

      sender.sendPayment(paymentInfo)

    case events.ReceiveMessage(sender, recipient, message) =>
      implicit val sendMessage = (delay: TimeDelta, msgRecipient: Node, newMessage: Message) =>
        scheduleEvent(
          delay + nodeReceiveTime(msgRecipient),
          events.ReceiveMessage(recipient, msgRecipient, newMessage)
        )

      message match {
        case message @ OpenChannel(_, _, _) => recipient.handleOpenChannel(sender, message)
        case message @ AcceptChannel(_, _, _) => recipient.handleAcceptChannel(sender, message)
        case message @ FundingCreated(_, _) => recipient.handleFundingCreated(sender, message)
        case message @ UpdateAddHTLC(_, _) => recipient.handleUpdateAddHTLC(sender, message)
        case message @ UpdateFulfillHTLC(_, _) => recipient.handleUpdateFulfillHTLC(sender, message)
        case message @ UpdateFailHTLC(_, _, _) => recipient.handleUpdateFailHTLC(sender, message)
      }

    case events.QueryNewPayment(node) =>
      for (paymentInfo <- node.newPayments()) {
        scheduleEvent(0, events.NewPayment(paymentInfo))
      }
      scheduleEvent(node.nextPaymentQuery, events.QueryNewPayment(node))
  }

  private def nodeReceiveTime(node: Node): TimeDelta = Util.drawExponential(node.meanNetworkLatency)

  private def createNetworkConnection(node: Node, scheduleEvent: (TimeDelta, Event) => Unit)
                                     (delay: TimeDelta, msgRecipient: Node, newMessage: Message): Unit =
    scheduleEvent(
      delay + nodeReceiveTime(msgRecipient),
      events.ReceiveMessage(node, msgRecipient, newMessage)
    )
}
