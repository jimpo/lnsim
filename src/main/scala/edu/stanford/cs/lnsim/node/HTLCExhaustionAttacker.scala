package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.{NetworkGraphView, RouteConstraints}
import edu.stanford.cs.lnsim.routing.{CriticalEdgeEstimator, Router}

/** HTLCExhaustionAttacker attempts to attack the network by performing loop attacks with long
  * circuits and very small amounts. The attacker seeks to exhaust the number of open accepted
  * HTLC slots.
  */
class HTLCExhaustionAttacker(id: NodeID,
                             params: NodeActor.Params,
                             private val attackParams: HTLCExhaustionAttacker.AttackParams,
                             private val edgeEstimator: CriticalEdgeEstimator,
                             output: ObservableOutput,
                             router: Router,
                             graphView: NetworkGraphView,
                             blockchain: BlockchainView)
  extends NodeActor(id, params, output, router, graphView, blockchain) {

  /** Fail to forward all payments immediately, so that attack nodes do not actually contribute to
    * network connectivity.
    */
  override def decideForwardHTLC(prevHop: HTLC, nextHop: HTLC)
  : (Option[RoutingError], Option[BlockNumber]) = {
    super.decideForwardHTLC(prevHop, nextHop) match {
      case ret @ (Some(_error), _) => ret
      case (None, _) => (Some(TemporaryChannelFailure), None)
    }
  }

  override def handleBootstrapEnd()(implicit ctx: NodeContext): Unit =
    sendAttackPayments()

  /** This runs an analysis on the network graph to determine which channels are the best targets
    * in order to maximize the number of payment failures. It uses the CriticalEdgeEstimator to
    * gather this information.
    */
  private def sendAttackPayments()(implicit ctx: NodeContext): Unit = {
    val analysis = edgeEstimator.analyzeIfNecessary(
      ctx.timestamp, graphView, new RouteConstraints().banNode(id)
    )
  }
}

object HTLCExhaustionAttacker {
  case class AttackParams(autoConnectChannelCapacity: Value,
                          attackerNodeIDs: Set[NodeID])
}
