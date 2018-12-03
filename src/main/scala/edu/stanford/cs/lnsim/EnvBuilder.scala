package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{TimeDelta, secondsToTimeDelta}
import edu.stanford.cs.lnsim.graph.NetworkGraph
import edu.stanford.cs.lnsim.log.StructuredLogging
import edu.stanford.cs.lnsim.node.{BlockchainView, DelayingActor, NodeActor}
import edu.stanford.cs.lnsim.routing.{MinimalFeeRouter, NetworkGraphView}
import edu.stanford.cs.lnsim.spec.SimulationSpec
import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

class EnvBuilder(private val spec: SimulationSpec,
                 private val blockchain: Blockchain) extends StructuredLogging {
  import EnvBuilder._

  def build(): Environment = {
    val graph = new NetworkGraph()
    val router = new MinimalFeeRouter(MaximumRoutingFee)
    val output = new LoggingOutput()

    val params = NodeActor.Params(
      EclairDefaults.ReserveToFundingRatio,
      EclairDefaults.DustLimitSatoshis,
      EclairDefaults.MaxHTLCInFlight,
      EclairDefaults.MaxAcceptedHTLCs,
      EclairDefaults.HTLCMinimum,
      EclairDefaults.MinDepthBlocks,
      EclairDefaults.FinalExpiryDelta,
      EclairDefaults.ExpiryDelta,
      EclairDefaults.FeeBase,
      EclairDefaults.FeeProportionalMillionths,
      LndDefaults.AutoPilotMinChannelSize,
      LndDefaults.AutoPilotMinChannelSize,
      LndDefaults.AutoPilotNumChannels,
      FundingTransactionWeight,
      CapacityMultiplier,
      OffChainPaymentTimeout,
    )

    // Create NodeActors for all nodes in the spec.
    val nodes = for (nodeSpec <- spec.nodes) yield {
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeSpec.id, blockchain)
      new NodeActor(nodeSpec.id, params, output, router, graphView, blockchainView)
    }
    val attackNodes = for (_ <- 1 to NumAttackNodes) yield {
      val nodeID = Util.randomUUID()
      val graphView = new NetworkGraphView(graph)
      val blockchainView = new BlockchainView(nodeID, blockchain)

      val attackParams = DelayingActor.AttackParams(
        numChannels = NumAttackChannelsPerNode,
        channelCapacity = CapacityPerAttackChannel,
      )
      logger.info(
        "msg" -> "Initializing attack node".toJson,
        "nodeID" -> nodeID.toJson,
        "budget" -> (attackParams.numChannels * attackParams.channelCapacity).toJson,
      )
      new DelayingActor(
        nodeID,
        params,
        attackParams,
        output,
        router,
        graphView,
        blockchainView,
      )
    }

    val nodeMap = (nodes ++ attackNodes).map(node => node.id -> node).toMap

    // Create NewPayment events for all transactions in the spec.
    val paymentEvents = for (transactionSpec <- spec.transactions) yield {
      val sender = nodeMap.getOrElse(transactionSpec.sender,
        throw new InvalidSpecError(s"Payment ${transactionSpec.paymentID} has unknown sender"))
      val recipient = nodeMap.getOrElse(transactionSpec.recipient,
        throw new InvalidSpecError(s"Payment ${transactionSpec.paymentID} has unknown recipient"))

      val payment = PaymentInfo(
        sender = sender,
        recipientID = recipient.id,
        amount = transactionSpec.amount,
        finalExpiryDelta = params.finalExpiryDelta,
        paymentID = transactionSpec.paymentID,
      )
      (transactionSpec.timestamp, events.NewPayment(payment))
    }

    // Create OpenChannels events for all channel budgets in the spec.
    val openChannelEvents = for (channelBudgetSpec <- spec.channelBudgets) yield {
      val node = nodeMap.getOrElse(channelBudgetSpec.node,
        throw new InvalidSpecError(s"ChannelBudget in spec has unknown sender ${channelBudgetSpec.node}"))

      (channelBudgetSpec.timestamp, events.OpenChannels(node, channelBudgetSpec.amount))
    }
    val bootstrapEnd = (spec.startTime, events.BootstrapEnd())

    val initialEvents = paymentEvents ++ openChannelEvents ++ Seq(bootstrapEnd)
    new Environment(nodeMap, initialEvents, blockchain)
  }
}

object EnvBuilder {
  /** This is the maximum that a node is willing to pay in routing fees. If the cost of a
    * transaction exceeds this amount, the node would prefer to send it on-chain anyway.
    *
    * Set to 0.0001 BTC.
    */
  val MaximumRoutingFee: Value = 10000000

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

  val NumAttackNodes: Int = 10
  val NumAttackChannelsPerNode: Int = 100
  val CapacityPerAttackChannel: Value = 100000000000L // 1 BTC
}
