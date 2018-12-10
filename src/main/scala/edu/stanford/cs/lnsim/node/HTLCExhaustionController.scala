package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.{Channel, NetworkGraphView, RouteConstraints}
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.routing.CriticalEdgeEstimator

import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

/** HTLCExhaustionController attempts to attack the network by performing loop attacks with long
  * circuits and very small amounts. The attacker seeks to exhaust the number of open accepted
  * HTLC slots.
  */
class HTLCExhaustionController(nodeID: NodeID,
                               graphView: NetworkGraphView,
                               params: NodeActor.Params,
                               attackParams: AutoPilotCaptureController.AttackParams,
                               attackerNodeIDs: Set[NodeID])
  extends AutoPilotCaptureController(params, attackParams) with StructuredLogging {

  /** Fail to forward all payments immediately, so that attack nodes do not actually contribute to
    * network connectivity.
    */
  override def forwardHTLC(prevHop: HTLC, nextHop: HTLC, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber]) = {
    super.forwardHTLC(prevHop, nextHop, blockNumber) match {
      case ret @ (Some(_error), _) => ret
      case (None, _) => (Some(TemporaryChannelFailure), None)
    }
  }

  override def bootstrapEndActions(): Seq[NodeAction] = {
    val startTime = System.currentTimeMillis()
    val estimator = new CriticalEdgeEstimator(_channel => 1.0)
    val analysis = estimator.analyze(graphView, new RouteConstraints().banNode(nodeID))
    val endTime = System.currentTimeMillis()

    logger.info(
      "msg" -> "Computed min cut".toJson,
      "trials" -> analysis.trials.toJson,
      "nodeCount" -> analysis.nodeCount.toJson,
      "edgeCount" -> analysis.edgeCount.toJson,
      "coreChannels" -> JsArray(analysis.coreChannels.map(_.toJson):_*),
      "elapsedTimeMS" -> (endTime - startTime).toJson,
    )

    super.bootstrapEndActions()
  }
}
