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

    case events.ReceiveHTLC(index, route) =>
      val hop = route.hops(index)
      val node = hop.recipient

      if (index + 1 < route.hops.length) {
        // Intermediate hop in the circuit.
        val nextHop = route.hops(index + 1)
        if (nextHop.sender != node) {
          // Maybe fail with BADONION instead?
          throw new MisbehavingNodeException("Invalid routing packet")
        }

        val (delay, maybeError) = node.forwardHTLC(hop, nextHop)
        if (delay == 0) {
          // Packet was dropped.
        }
        val event = maybeError match {
          case Some(error) =>
            // Fail the packet, returning to sender.
            val latency = nodeReceiveTime(hop.sender)
            (delay + latency, events.FailHTLC(index, route, error))

          case None =>
            // Forward the packet to the next hop.
            val latency = nodeReceiveTime(nextHop.recipient)
            (delay + latency, events.ReceiveHTLC(index + 1, route))
        }
        List(event)
      } else {
        // Final hop in the circuit.
        val (delay, maybeError) = node.acceptHTLC(hop, route.finalHop)
        val event = maybeError match {
          case Some(error) => events.FailHTLC(index, route, error)
          case None => events.FulfillHTLC(index, route)
        }

        val latency = nodeReceiveTime(hop.sender)
        List((delay + latency, event))
      }

    case events.FailHTLC(index, route, error) =>
      val hop = route.hops(index)
      val node = hop.sender

      if (index > 0) {
        // Intermediate hop in the circuit.
        val latency = nodeReceiveTime(hop.sender)
        val delay = node.failHTLC(hop)
        List((delay + latency, events.FailHTLC(index - 1, route, error)))
      } else {
        // Error made it back to the original sender.
        node.failPayment(hop, error)
        List()
      }
  }

  private def nodeReceiveTime(node: Node): TimeDelta = Util.drawExponential(node.meanNetworkLatency, rand)
}
