package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim.graph.{NetworkGraphView, RouteConstraints}
import edu.stanford.cs.lnsim.{ChannelID, NodeID, ObservableOutput}
import edu.stanford.cs.lnsim.routing.{CriticalEdgeEstimator, Router}
import DisablingAttacker._
import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}

import scala.collection.mutable

import spray.json._
import spray.json.DefaultJsonProtocol._
import edu.stanford.cs.lnsim.JSONProtocol._

/** The DisablingAttacker implements an attacker with the special ability to disable any n channels
  * on the network it wants to at a time. This is a far-reaching power which can be used to
  * evaluate the effectiveness of the critical edge estimation algorithm and be viewed as sort of
  * an upper bound on the power of a loop attacker that targets specific channels.
  *
  * This node does not actually participate in the network aside from observing and exercising this
  * supernatural channel-disabling power.
  */
class DisablingAttacker(id: NodeID,
                        params: NodeActor.Params,
                        private val attackParams: AttackParams,
                        private val edgeEstimator: CriticalEdgeEstimator,
                        output: ObservableOutput,
                        router: Router,
                        graphView: NetworkGraphView,
                        blockchain: BlockchainView)
  extends NodeActor(id, params, output, router, graphView, blockchain) {

  var disabledChannels: List[ChannelID] = List()

  override def handleBootstrapEnd()(implicit ctx: NodeContext): Unit =
    for (time <- ctx.timestamp to attackParams.stopTime by attackParams.interval) {
      ctx.scheduleAction(time - ctx.timestamp, AttackStep)
      ctx.scheduleAction(ctx.timestamp - attackParams.stopTime, LogAttackState)
    }

  override def handleAction(action: NodeAction)
                           (implicit ctx: NodeContext): Unit = action match {
    case AttackStep => disableCriticalChannels(ctx.timestamp)
    case LogAttackState => logCriticalEdges(ctx.timestamp)
    case _ => super.handleAction(action)
  }

  private def disableCriticalChannels(timestamp: Timestamp): Unit = {
    // Re-enable channels from last run.
    for (channelID <- disabledChannels ;
         channel <- graphView.channels(channelID)) {
      graphView.updateChannel(channel.copy(disabled = false))
    }

    // Determine new set of most critical channels.
    edgeEstimator.analyze(timestamp, graphView, new RouteConstraints())
    val analysis = edgeEstimator.lastAnalysis

    val targetChannelQueue = mutable.PriorityQueue.empty[(Int, ChannelID)]
    for ((channel, weight) <- analysis.coreChannels.iterator) {
      if (targetChannelQueue.size < attackParams.numTargetChannels) {
        targetChannelQueue.enqueue((-weight, channel))
      } else if (weight > -targetChannelQueue.head._1) {
        targetChannelQueue.dequeue()
        targetChannelQueue.enqueue((-weight, channel))
      }
    }

    disabledChannels = targetChannelQueue.dequeueAll.map(_._2).toList

    logger.info(
      "msg" -> "Attacker disabling channels".toJson,
      "count" -> disabledChannels.length.toJson,
    )

    // Disable most recent critical channels.
    for (channelID <- disabledChannels ;
         channel <- graphView.channels(channelID)) {
      graphView.updateChannel(channel.copy(disabled = true))
    }
  }

  private def logCriticalEdges(timestamp: Timestamp): Unit = {
    edgeEstimator.analyze(timestamp, graphView, new RouteConstraints())

    val coreChannels = edgeEstimator.lastAnalysis.coreChannels.toIterator.map(kv => {
      val (channelID, weight) = kv
      JsArray(channelID.toJson, weight.toJson)
    })
    logger.info(
      "msg" -> "Estimated critical channels".toJson,
      "nodeID" -> id.toJson,
      "channels" -> coreChannels.toSeq.toJson,
    )
  }
}

object DisablingAttacker {
  case class AttackParams(numTargetChannels: Int,
                          stopTime: Timestamp,
                          interval: TimeDelta)
}
