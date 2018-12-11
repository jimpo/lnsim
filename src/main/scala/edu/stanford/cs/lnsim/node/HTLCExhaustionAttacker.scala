package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.des.{TimeDelta, Timestamp}
import edu.stanford.cs.lnsim.graph.{NetworkGraphView, RouteConstraints}
import edu.stanford.cs.lnsim.routing.{CriticalEdgeEstimator, Router}
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._
import HTLCExhaustionAttacker._

/** HTLCExhaustionAttacker attempts to attack the network by performing loop attacks with long
  * circuits and very small amounts. The attacker seeks to exhaust the number of open accepted
  * HTLC slots.
  */
class HTLCExhaustionAttacker(id: NodeID,
                             params: NodeActor.Params,
                             private val attackParams: AttackParams,
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
      case (None, _) => (Some(TemporaryChannelFailure(ChannelView.Error.Inactive)), None)
    }
  }

  /** Hold onto all HTLCs send to this node (which must have come from another attacker-controlled
    * node) for as long as possible.
    */
  override def decideAcceptHTLC(htlc: HTLC, finalHop: FinalHop)(implicit ctx: NodeContext)
  : (Option[RoutingError], Option[BlockNumber]) = {
    super.decideAcceptHTLC(htlc, finalHop) match {
      case ret @ (Some(_error), _) => ret
      case (None, _) =>
        logger.debug(
          "msg" -> "Loop attacker withholding payment".toJson,
          "id" -> id.toJson,
          "paymentID" -> htlc.paymentID.toJson,
        )

        // Immediately schedule another attacker payment to be sent when the last one succeeds.
        ctx.scheduleAction(0, AttackStep)

        (Some(LoopAttackSuccess), Some(htlc.expiry - params.minExpiry))
    }
  }

  override def handleBootstrapEnd()(implicit ctx: NodeContext): Unit = {
    openNewChannels(params.autoConnectNumChannels * attackParams.autoConnectChannelCapacity)
    for (time <- ctx.timestamp until attackParams.stopTime by attackParams.newRouteInterval) {
      ctx.scheduleAction(time - ctx.timestamp, AttackStep)
    }
    ctx.scheduleAction(ctx.timestamp - attackParams.stopTime - 1, LogAttackState)
  }

  override def handleAction(action: NodeAction)
                           (implicit ctx: NodeContext): Unit = action match {
    case AttackStep => sendAttackPayments()
    case LogAttackState => logCriticalEdges(ctx.timestamp)
    case _ => super.handleAction(action)
  }

  override def openNewChannels(budget: Value)(implicit ctx: NodeContext): Unit = {
    val autoConnect = new AutoPilotCapture(
      params.autoConnectNumChannels, attackParams.autoConnectChannelCapacity
    )
    for ((targetNodeID, capacity) <- autoConnect.newChannels(id, budget, graphView)) {
      initiateChannelOpen(targetNodeID, capacity, maybePendingPayment = None)
    }
  }

  /** This runs an analysis on the network graph to determine which channels are the best targets
    * in order to maximize the number of payment failures. It uses the CriticalEdgeEstimator to
    * gather this information.
    */
  private def sendAttackPayments()(implicit ctx: NodeContext): Unit = {
    val edgeEstimatorConstraints = attackParams.attackerNodeIDs
      .foldLeft(new RouteConstraints()) { (constraints, id) => constraints.banNode(id) }
    edgeEstimator.analyzeIfNecessary(ctx.timestamp, graphView, edgeEstimatorConstraints)

    val payment = PaymentInfo(
      sender = id,
      recipient = id,
      amount = params.htlcMinimum,
      finalExpiryDelta = attackParams.loopFinalExpiryDelta,
      paymentID = Util.randomUUID(),
      fallbackOnChain = false,
    )

    // val localConstraints = addIncomingLocalConstraints(new RouteConstraints(), payment.amount)
    val localConstraints = new RouteConstraints()

    logger.debug(
      "msg" -> "Executing loop attack payment".toJson,
      "nodeID" -> id.toJson,
      "paymentID" -> payment.paymentID.toJson,
    )
    val pendingPayment = PendingPayment(
      payment,
      ctx.timestamp,
      tries = 1,
      route = Nil,
      constraints = localConstraints,
    )
    executePayment(pendingPayment)
  }

  private def logCriticalEdges(timestamp: Timestamp): Unit = {
    val edgeEstimatorConstraints = attackParams.attackerNodeIDs
      .foldLeft(new RouteConstraints()) { (constraints, id) => constraints.banNode(id) }
    edgeEstimator.analyze(timestamp, graphView, edgeEstimatorConstraints)

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

object HTLCExhaustionAttacker {
  private val RoutingTime: TimeDelta = 500

  case class AttackParams(autoConnectChannelCapacity: Value,
                          attackerNodeIDs: Set[NodeID],
                          newRouteInterval: TimeDelta,
                          stopTime: Timestamp,
                          loopFinalExpiryDelta: BlockDelta)
}
