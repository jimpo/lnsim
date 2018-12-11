package edu.stanford.cs.lnsim.routing

import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraphView, RouteConstraints}
import edu.stanford.cs.lnsim.routing.CriticalEdgeRouter.RandomWalkPath
import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.log.StructuredLogging

import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

import scala.annotation.tailrec

class CriticalEdgeRouter(lastMileRouter: Router,
                         edgeEstimator: CriticalEdgeEstimator,
                         maxFee: Value,
                         maxHops: Int,
                         maxExpiry: BlockDelta) extends Router with StructuredLogging {

  override def findPath(paymentInfo: PaymentInfo,
                        graph: NetworkGraphView,
                        constraints: RouteConstraints): List[Channel] = {
    val channelValues = edgeEstimator.lastAnalysis.coreChannels

    val initialPath = RandomWalkPath(
      lastNode = paymentInfo.sender,
      hops = 0,
      expiry = paymentInfo.finalExpiryDelta,
      channels = Nil,
      constraints = graph.constraints + constraints,
    )
    val finalPath = randomWalk(
      paymentInfo.sender, paymentInfo.recipient, channelValues, graph, initialPath
    )

    if (finalPath.lastNode == paymentInfo.recipient) {
      return finalPath.channels.reverse
    }

    logger.info(
      "msg" -> "Found a random walk path".toJson,
      "paymentID" -> paymentInfo.paymentID.toJson,
      "length" -> finalPath.channels.length.toJson,
    )

    completeRandomWalkPath(graph, finalPath, paymentInfo)
  }

  @tailrec
  private def randomWalk(startNode: NodeID,
                         targetNode: NodeID,
                         channelValues: Map[ChannelID, Int],
                         graph: NetworkGraphView,
                         path: RandomWalkPath): RandomWalkPath = {
    val channels = graph.node(path.lastNode).map(_.channelsIterator).getOrElse(Iterator.empty)
    val bestChannel = try {
      channels
        .filter { channel => path.constraints.allowChannel(channel) }
        .maxBy { channel => channelValues.getOrElse(channel.id, 0) }
    } catch {
      // In case there are no possible next channels
      case e: UnsupportedOperationException if e.getMessage == "empty.maxBy" => return path
    }

    val newConstraints = if (bestChannel.source == targetNode) {
      path.constraints
    } else {
      path.constraints.banNode(bestChannel.source)
    }
    val newPath = RandomWalkPath(
      lastNode = bestChannel.target,
      hops = path.hops + 1,
      expiry = path.expiry + bestChannel.expiryDelta,
      channels = bestChannel :: path.channels,
      constraints = newConstraints,
    )
    if (newPath.hops > maxHops || newPath.expiry > maxExpiry) {
      path
    } else if (newPath.lastNode == targetNode) {
      newPath
    } else {
      randomWalk(startNode, targetNode, channelValues, graph, newPath)
    }
  }

  @tailrec
  private def completeRandomWalkPath(graph: NetworkGraphView,
                                     path: RandomWalkPath,
                                     payment: PaymentInfo): List[Channel] = {
    path.channels match {
      case lastChannel :: firstChannels =>
        val lastMilePayment = payment.copy(
          sender = lastChannel.target,
          finalExpiryDelta = path.expiry
        )
        val lastMilePath = lastMileRouter.findPath(lastMilePayment, graph, path.constraints)
        if (lastMilePath.nonEmpty) {
          path.channels.reverse ++ lastMilePath
        } else{
          val truncatedPath = RandomWalkPath(
            lastNode = lastChannel.source,
            hops = path.hops - 1,
            expiry = path.expiry - lastChannel.expiryDelta,
            channels = firstChannels,
            constraints = path.constraints,
          )
          completeRandomWalkPath(graph, truncatedPath, payment)
        }
      case Nil => Nil
    }
  }
}

object CriticalEdgeRouter {
  private case class RandomWalkPath(lastNode: NodeID,
                                    hops: Int,
                                    expiry: BlockDelta,
                                    channels: List[Channel],
                                    constraints: RouteConstraints)
}
