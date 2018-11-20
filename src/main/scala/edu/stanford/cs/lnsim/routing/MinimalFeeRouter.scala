package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.Channel

import scala.collection.mutable

/**
  * The MinimalFeeRouter uses Dijkstra's algorithm to find the route fulfilling the payment
  * that minimizes fees while respecting the route constraints.
  */
class MinimalFeeRouter(maxFee: Value) extends Router {

  override def findPath(paymentInfo: PaymentInfo,
                        graph: NetworkGraphView,
                        localConstraints: RouteConstraints): List[Channel] = {
    val source = paymentInfo.sender.id
    val target = paymentInfo.recipientID
    val constraints = graph.constraints + localConstraints

    val distances = mutable.HashMap.empty[NodeID, (Value, Channel, Boolean)]
    val queue = mutable.PriorityQueue.empty[(Value, NodeID)]

    queue.enqueue((0, source))
    distances.put(source, (0, null, false))

    while (queue.nonEmpty) {
      val (dist, nodeID) = queue.dequeue()
      val (_, prev, visited) = distances(nodeID)
      if (!visited) {
        distances(nodeID) = (dist, prev, true)
        if (nodeID == target) {
          return recoverPath(source, target, distances)
        }

        val channels = graph.node(nodeID).map(_.channels.valuesIterator).getOrElse(Iterator.empty)
        for (channel <- channels) {
          if (checkChannel(channel, paymentInfo.amount, constraints)) {
            val edgeWeight = channel.lastUpdate.feeBase +
              (paymentInfo.amount * channel.lastUpdate.feeProportionalMillionths / 1000000.0).toLong
            val newEstimate = dist + edgeWeight
            val betterPath = distances.get(channel.target).map(newEstimate < _._1).getOrElse(true)
            if (betterPath) {
              distances(channel.target) = (newEstimate, channel, false)
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
    !channel.lastUpdate.disabled &&
      amount >= channel.lastUpdate.htlcMinimum &&
      amount <= channel.lastUpdate.htlcMaximum &&
      constraints.allowChannel(channel)
  )

  private def recoverPath(source: NodeID, target: NodeID,
                          distances: mutable.Map[NodeID, (Value, Channel, Boolean)]): List[Channel] = {

    var nodeID = target
    var channels: List[Channel] = Nil
    while (nodeID != source) {
      val (_, channel, _) = distances(nodeID)
      channels = channel :: channels
      nodeID = channel.source
    }
    channels
  }
}
