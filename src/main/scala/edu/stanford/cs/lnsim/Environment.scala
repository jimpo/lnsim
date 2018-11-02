package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.util.Random

class Environment(private val rand: Random,
                  val blockchain: Blockchain,
                  val networkGraph: NetworkGraph) extends des.Environment {

  override type Event = events.Base

  override def initialEvent(): Event = events.Start

  override def processEvent(event: Event): List[(TimeDelta, Event)] = event match {
    case events.Start => List((blockchain.nextBlockTime(), events.NewBlock(0)))

    case events.NewBlock(number) =>
      val receiveBlockEvents = for (node <- networkGraph.nodeIterator)
        yield (nodeReceiveTime(node), events.ReceiveBlock(node, number))
      receiveBlockEvents.toList ::: List((blockchain.nextBlockTime(), events.NewBlock(number + 1)))

    case events.ReceiveBlock(_node, _number) => List.empty

    case events.NewPayment(sender, paymentInfo) =>
      val routingPacket = sender.route(paymentInfo)
      routingPacket.hops.headOption match {
        case Some(firstHop) =>
          if (firstHop.sender != sender) {
            throw new MisbehavingNodeException("First hop sender is not payment sender")
          }
          val receiveEvent = events.ReceiveHTLC(0, routingPacket)
          List((nodeReceiveTime(firstHop.recipient), receiveEvent))

        case None => List.empty
      }

    case events.ReceiveMessage(sender, recipient, message) =>
      message match {
        case message @ UpdateAddHTLC(_, _) => recipient.handleUpdateAddHTLC(sender, message)
        case message @ UpdateFulfillHTLC(_, _) => recipient.handleUpdateFulfillHTLC(sender, message)
        case message @ UpdateFailHTLC(_, _, _) => recipient.handleUpdateFailHTLC(sender, message)
      }

      val newEvents = for ((delay, node, newMessage) <- recipient.drainPacketQueue()) yield {
        val latency = nodeReceiveTime(node)
        (delay + latency, events.ReceiveMessage(recipient, node, newMessage))
      }
      newEvents.toList
  }

  private def nodeReceiveTime(node: Node): TimeDelta = Util.drawExponential(node.meanNetworkLatency, rand)
}
