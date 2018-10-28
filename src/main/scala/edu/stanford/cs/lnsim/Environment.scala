package edu.stanford.cs.lnsim

import scala.util.Random

class Environment(private val rand: Random,
                  val blockchain: Blockchain,
                  val networkGraph: NetworkGraph) extends des.Environment {

  override type Event = events.Base

  override def initialEvent(): Event = events.Start

  override def processEvent(event: Event): List[(Int, Event)] = event match {
    case events.Start => List((blockchain.nextBlockTime(), events.NewBlock(0)))

    case events.NewBlock(number) =>
      val receiveBlockEvents = for (node <- networkGraph.nodeIterator)
        yield (nodeReceiveTime(node), events.ReceiveBlock(node, number))
      receiveBlockEvents.toList ::: List((blockchain.nextBlockTime(), events.NewBlock(number + 1)))

    case events.ReceiveBlock(_node, _number) => List.empty

    case events.NewPayment(sender, paymentInfo) =>
      val route = sender.route(paymentInfo)
      route.headOption match {
        case Some(firstHop) => firstHop.channel match {
          case Some(channel) =>
            if (channel.sender(firstHop.direction) != sender) {
              throw new MisbehavingNodeException("First hop sender is not payment sender")
            }
            val recipient = channel.recipient(firstHop.direction)
            val routingPacket = ForwardRoutingPacket(route, valid = true)
            val receiveEvent = events.ReceiveHTLC(recipient, 0, routingPacket)
            List((nodeReceiveTime(recipient), receiveEvent))

          case None => throw new MisbehavingNodeException("First hop has no recipient")
        }
        case None => List.empty
      }
  }

  private def nodeReceiveTime(node: Node): Int = Util.drawExponential(node.meanNetworkLatency, rand)
}
