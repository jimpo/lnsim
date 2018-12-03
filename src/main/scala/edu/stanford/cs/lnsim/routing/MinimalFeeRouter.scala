package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.Channel

import scala.collection.mutable

/**
  * The MinimalFeeRouter uses Dijkstra's algorithm to find the route fulfilling the payment
  * that minimizes fees while respecting the route constraints.
  */
class MinimalFeeRouter(maxFee: Value, maxHops: Int) extends Router {

  override def findPath(paymentInfo: PaymentInfo,
                        graph: NetworkGraphView,
                        localConstraints: RouteConstraints): List[Channel] = {
    val source = paymentInfo.sender.id
    val target = paymentInfo.recipientID
    val constraints = graph.constraints + localConstraints

    val distances = mutable.HashMap.empty[NodeID, (Value, Int, Channel, Boolean)]
    val queue = mutable.PriorityQueue.empty[(Value, NodeID)]

    queue.enqueue((0, source))
    distances.put(source, (0, 0, null, false))

    while (queue.nonEmpty) {
      val (_, nodeID) = queue.dequeue()
      val (dist, hops, prev, visited) = distances(nodeID)
      if (!visited) {
        distances(nodeID) = (dist, hops, prev, true)
        if (nodeID == target) {
          return recoverPath(source, target, distances)
        }

        val channels = graph.node(nodeID).map(_.channels.valuesIterator).getOrElse(Iterator.empty)
        for (channel <- channels) {
          val newEstimate = dist + channel.fee(paymentInfo.amount)
          if (newEstimate <= maxFee && hops <= maxHops &&
            checkChannel(channel, paymentInfo.amount, constraints)) {

            val betterPath = distances.get(channel.target).forall(newEstimate < _._1)
            if (betterPath) {
              distances(channel.target) = (newEstimate, hops + 1, channel, false)
              queue.enqueue((newEstimate, channel.target))
            }
          }
        }
      }
    }
    List.empty
  }

  private def checkChannel(channel: Channel,
                           amount: Value,
                           constraints: RouteConstraints): Boolean = (
    !channel.disabled &&
      amount >= channel.htlcMinimum &&
      amount <= channel.htlcMaximum &&
      constraints.allowChannel(channel)
  )

  private def recoverPath(source: NodeID, target: NodeID,
                          distances: mutable.Map[NodeID, (Value, Int, Channel, Boolean)]): List[Channel] = {

    var nodeID = target
    var channels: List[Channel] = Nil
    while (nodeID != source) {
      val (_, _, channel, _) = distances(nodeID)
      channels = channel :: channels
      nodeID = channel.source
    }
    channels
  }
}
