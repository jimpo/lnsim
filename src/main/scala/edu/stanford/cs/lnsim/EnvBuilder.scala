package edu.stanford.cs.lnsim
import edu.stanford.cs.lnsim.graph.{NetworkGraph, NetworkGraphView}
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.node._
import edu.stanford.cs.lnsim.routing.{CriticalEdgeEstimator, CriticalEdgeRouter, MinimalFeeRouter, Router}
import edu.stanford.cs.lnsim.spec.SimulationSpec
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._
import edu.stanford.cs.lnsim.des.{TimeDelta, secondsToTimeDelta}

class EnvBuilder(private val output: ObservableOutput,
                 private val spec: SimulationSpec,
                 private val blockchain: Blockchain) extends StructuredLogging {
  import EnvBuilder._

  def build(numDelayAttackNodes: Int = 0,
            numLoopAttackNodes: Int = 0,
            disableCriticalChannels: Int = 0): Environment = {
    val graph = new NetworkGraph()
    val router = new MinimalFeeRouter(MaximumRoutingFee, MaxRoutingHops, EclairDefaults.MaxExpiry)

    val params = NodeActor.Params(
      EclairDefaults.ReserveToFundingRatio,
      EclairDefaults.DustLimitSatoshis,
      MaxHTLCInFlight,
      EclairDefaults.MaxAcceptedHTLCs,
      EclairDefaults.HTLCMinimum,
      EclairDefaults.MinDepthBlocks,
      EclairDefaults.ExpiryDelta,
      EclairDefaults.FinalExpiryDelta,
      EclairDefaults.FeeBase,
      EclairDefaults.FeeProportionalMillionths,
      EclairDefaults.MinFundingAmount,
      EclairDefaults.MinExpiry,
      EclairDefaults.MaxExpiry,
      FundingTransactionWeight,
      CapacityMultiplier,
      LndDefaults.AutoPilotNumChannels,
      Math.max(LndDefaults.AutoPilotMinChannelSize, EclairDefaults.MinFundingAmount),
      OffChainPaymentTimeout,
    )

    // Create NodeActors for all nodes in the spec.
    val honestNodes = buildHonestNodes(graph, router, params)
    val delayAttackNodes = buildDelayAttackNodes(numDelayAttackNodes, graph, router, params)
    val loopAttackNodes = buildHTLCLoopAttackNodes(numLoopAttackNodes, graph, router, params)
    val disablingAttacker = if (disableCriticalChannels > 0) {
      Seq(buildDisablingAttackNode(disableCriticalChannels, graph, router, params))
    } else {
      Seq()
    }

    val nodeMap = (honestNodes ++ delayAttackNodes ++ loopAttackNodes ++ disablingAttacker)
      .map(node => node.id -> node).toMap

    // Create NewPayment events for all transactions in the spec.
    val paymentEvents = for (transactionSpec <- spec.transactions) yield {
      val sender = nodeMap.getOrElse(transactionSpec.sender,
        throw new InvalidSpecError(s"Payment ${transactionSpec.paymentID} has unknown sender"))
      val recipient = nodeMap.getOrElse(transactionSpec.recipient,
        throw new InvalidSpecError(s"Payment ${transactionSpec.paymentID} has unknown recipient"))

      val payment = PaymentInfo(
        sender = sender.id,
        recipient = recipient.id,
        amount = transactionSpec.amount,
        finalExpiryDelta = params.finalExpiryDelta,
        paymentID = transactionSpec.paymentID,
        fallbackOnChain = true,
      )
      (transactionSpec.timestamp, events.NewPayment(payment))
    }

    // Create OpenChannels events for all channel budgets in the spec.
    val openChannelEvents = for (channelBudgetSpec <- spec.channelBudgets) yield {
      val node = nodeMap.getOrElse(channelBudgetSpec.node,
        throw new InvalidSpecError(s"ChannelBudget in spec has unknown sender ${channelBudgetSpec.node}"))

      (channelBudgetSpec.timestamp, events.OpenChannels(node, channelBudgetSpec.amount))
    }
    val bootstrapEnd = (spec.startTime - secondsToTimeDelta(60 * 60), events.BootstrapEnd())

    val initialEvents = paymentEvents ++ openChannelEvents ++ Seq(bootstrapEnd)
    new Environment(nodeMap, initialEvents, blockchain)
  }

  private def buildHonestNodes(graph: NetworkGraph,
                               router: MinimalFeeRouter,
                               params: NodeActor.Params): List[NodeActor] = {
    for (nodeSpec <- spec.nodes) yield {
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeSpec.id, blockchain)
      new NodeActor(nodeSpec.id, params, output, router, graphView, blockchainView)
    }
  }

  private def buildDelayAttackNodes(numNodes: BlockDelta,
                                    graph: NetworkGraph,
                                    router: MinimalFeeRouter,
                                    params: NodeActor.Params): Seq[NodeActor] = {
    val attackerParams = params.copy(
      feeBase = 0,
      feeProportionalMillionths = 0,
      autoConnectNumChannels = NumAttackChannelsPerNode,
    )
    val attackParams = NaiveDelayingAttacker.AttackParams(CapacityPerAttackChannel)
    for (_ <- 1 to numNodes) yield {
      val nodeID = Util.randomUUID()
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeID, blockchain)

      logger.info(
        "msg" -> "Initializing attack node".toJson,
        "nodeID" -> nodeID.toJson,
        "budget" -> (params.autoConnectNumChannels * attackParams.autoConnectChannelCapacity).toJson,
      )
      new NaiveDelayingAttacker(
        nodeID,
        attackerParams,
        attackParams,
        output,
        router,
        graphView,
        blockchainView,
      )
    }
  }

  private def buildHTLCLoopAttackNodes(numNodes: BlockDelta,
                                       graph: NetworkGraph,
                                       router: Router,
                                       params: NodeActor.Params): Seq[NodeActor] = {
    val attackNodeIDs = (1 to numNodes).map(_ => Util.randomUUID()).toSet
    val attackerParams = params.copy(
      autoConnectNumChannels = NumAttackChannelsPerNode,
    )
    var attackParams = HTLCExhaustionAttacker.AttackParams(
      CapacityPerAttackChannel,
      attackNodeIDs,
      LoopAttackInterval,
      spec.endTime,
      LoopFinalExpiryDelta,
    )
    val edgeEstimator = new CriticalEdgeEstimator(AttackerAnalysisInterval, _channel => 1.0)
    val loopRouter = new CriticalEdgeRouter(
      router, edgeEstimator, MaximumRoutingFee, MaxRoutingHops, EclairDefaults.MaxExpiry
    )
    val nodes = for (nodeID <- attackNodeIDs.iterator) yield {
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeID, blockchain)

      logger.info(
        "msg" -> "Initializing attack node".toJson,
        "nodeID" -> nodeID.toJson,
        "budget" -> (params.autoConnectNumChannels * attackParams.autoConnectChannelCapacity).toJson,
      )
      new HTLCExhaustionAttacker(
        nodeID,
        attackerParams,
        attackParams,
        edgeEstimator,
        output,
        loopRouter,
        graphView,
        blockchainView,
      )
    }
    nodes.toSeq
  }

  private def buildDisablingAttackNode(numTargetChannels: Int,
                                       graph: NetworkGraph,
                                       router: Router,
                                       params: NodeActor.Params): NodeActor = {
    val attackParams = DisablingAttacker.AttackParams(
      numTargetChannels = numTargetChannels,
      stopTime = spec.endTime,
      interval = secondsToTimeDelta(10 * 60),
    )
    val edgeEstimator = new CriticalEdgeEstimator(AttackerAnalysisInterval, _channel => 1.0)
    val graphView = new NetworkGraphView(graph)

    val nodeID = Util.randomUUID()
    val blockchainView = new BlockchainView(nodeID, blockchain)

    new DisablingAttacker(
      nodeID,
      params,
      attackParams,
      edgeEstimator,
      output,
      router,
      graphView,
      blockchainView,
    )
  }
}

object EnvBuilder {
  /** This is the maximum that a node is willing to pay in routing fees. If the cost of a
    * transaction exceeds this amount, the node would prefer to send it on-chain anyway.
    *
    * Set to 0.0001 BTC.
    */
  val MaximumRoutingFee: Value = 10000000

  /** Maximum number of hops allowed in a payment route. This is specified in BOLT 4.
    */
  val MaxRoutingHops: Int = 20

  /** Raise the MaxHTLCInFlight to a very high amount to accommodate large payments. */
  val MaxHTLCInFlight: Value = 1000000000000L // 10 BTC

  /** Assume the weight of the funding transaction in BOLT 3: Appendix B test vectors, which is 928.
    */
  val FundingTransactionWeight: Int = 928

  /** Assume that when nodes open new direct channels in order complete payments, that the capacity
    * of the channel is this multiple of the payment amount.
    */
  val CapacityMultiplier: Int = 2

  /** If a payment cannot be completed off-chain within this amount of time, open a direct channel
    * to guarantee payment delivery.
    */
  val OffChainPaymentTimeout: TimeDelta = secondsToTimeDelta(30 * 60)

  val NumAttackChannelsPerNode: Int = 1000
  val CapacityPerAttackChannel: Value = 1000000000000L // EclairDefaults.MinFundingAmount
  val AttackerAnalysisInterval: TimeDelta = secondsToTimeDelta(30 * 60)

  /** Final expiry delta in a loop attack route. Increase it in order to provide deniability to
    * attacker.
    */
  val LoopFinalExpiryDelta: BlockDelta =
    EclairDefaults.ExpiryDelta + EclairDefaults.FinalExpiryDelta

  val LoopAttackInterval: TimeDelta = secondsToTimeDelta(10 * 60)
}
