package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraphView, Node, RouteConstraints}

import scala.collection.mutable

/**
  * The MinimalFeeRouter uses Dijkstra's algorithm to find the route fulfilling the payment
  * that minimizes fees while respecting the route constraints.
  */
class MinimalFeeRouter(maxFee: Value, maxHops: Int, maxExpiry: BlockDelta) extends Router {
  import MinimalFeeRouter._

  override def findPath(paymentInfo: PaymentInfo,
                        graph: NetworkGraphView,
                        localConstraints: RouteConstraints): List[Channel] = {
    val source = paymentInfo.sender.id
    val target = paymentInfo.recipientID
    val constraints = graph.constraints + localConstraints

    val paths = mutable.HashMap.empty[NodeID, PathInfo]
    val queue = mutable.PriorityQueue.empty[(Value, NodeID)]

    queue.enqueue((0, source))
    paths.put(source, PathInfo(
      lastChannel = None,
      fee = 0,
      hops = 0,
      expiry = paymentInfo.finalExpiryDelta,
      isBest = false)
    )

    while (queue.nonEmpty) {
      val (_, nodeID) = queue.dequeue()
      val path = paths(nodeID)
      if (!path.isBest) {
        paths(nodeID) = path.copy(isBest = true)
        if (nodeID == target) {
          return recoverPath(source, target, paths)
        }

        val node = graph.node(nodeID).getOrElse(new Node(nodeID))
        node.foreachChannel(channel => {
          val newPath = PathInfo(
            lastChannel = Some(channel),
            fee = path.fee + channel.fee(paymentInfo.amount),
            hops = path.hops + 1,
            expiry = path.expiry + channel.expiryDelta,
            isBest = false,
          )
          if (checkPath(newPath) && checkChannel(channel, paymentInfo.amount)) {
            val betterPath = paths.get(channel.target).forall(newPath.fee < _.fee)
            // Delay the checkChannel call because it is slow and less likely to be false
            if (betterPath && constraints.allowChannel(channel)) {
              paths(channel.target) = newPath
              queue.enqueue((newPath.fee, channel.target))
            }
          }
        })
      }
    }
    List.empty
  }

  private def checkPath(path: PathInfo): Boolean =
    path.fee <= maxFee && path.hops <= maxHops && path.expiry <= maxExpiry

  private def checkChannel(channel: Channel, amount: Value): Boolean =
    !channel.disabled &&
      amount >= channel.htlcMinimum &&
      amount <= channel.htlcMaximum

  private def recoverPath(source: NodeID, target: NodeID, paths: mutable.Map[NodeID, PathInfo])
  : List[Channel] = {
    var nodeID = target
    var channels: List[Channel] = Nil
    while (true) {
      val path = paths(nodeID)
      path.lastChannel match {
        case Some(channel) =>
          channels = channel :: channels
          nodeID = channel.source
        case None =>
          return channels
      }
    }
    channels
  }
}

object MinimalFeeRouter {
  /** This is used internally to track information about each node in the Dijkstra's execution.
    */
  private case class PathInfo(lastChannel: Option[Channel],
                              fee: Value,
                              hops: Int,
                              expiry: BlockDelta,
                              isBest: Boolean)
}
