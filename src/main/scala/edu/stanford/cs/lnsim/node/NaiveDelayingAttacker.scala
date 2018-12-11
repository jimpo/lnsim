package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.NetworkGraphView
import edu.stanford.cs.lnsim.routing.Router

import NaiveDelayingAttacker._

/** A NaiveDelayingAttacker implements an opportunistic attacker strategy where it will reduce fees
  * and delay any HTLCs forwarded through it for the maximum amount of time before the HTLC
  * expires without forcing a unilateral on-chain close.
 */
class NaiveDelayingAttacker(id: NodeID,
                            params: NodeActor.Params,
                            private val attackParams: AttackParams,
                            output: ObservableOutput,
                            router: Router,
                            graphView: NetworkGraphView,
                            blockchain: BlockchainView)
  extends NodeActor(id, params, output, router, graphView, blockchain) {

  override def decideForwardHTLC(prevHop: HTLC, nextHop: HTLC)
  : (Option[RoutingError], Option[BlockNumber]) = {
    super.decideForwardHTLC(prevHop, nextHop) match {
      case ret @ (Some(_error), _) => ret
      case (None, _) => (
        Some(TemporaryChannelFailure(ChannelView.Error.Inactive)),
        Some(prevHop.expiry - params.minExpiry)
      )
    }
  }

  override def openNewChannels(budget: Value)(implicit ctx: NodeContext): Unit = {
    val autoConnect = new AutoPilotCapture(
      params.autoConnectNumChannels, attackParams.autoConnectChannelCapacity
    )
    for ((targetNodeID, capacity) <- autoConnect.newChannels(id, budget, graphView)) {
      initiateChannelOpen(targetNodeID, capacity, maybePendingPayment = None)
    }
  }

  override def handleBootstrapEnd()(implicit ctx: NodeContext): Unit =
    openNewChannels(params.autoConnectNumChannels * attackParams.autoConnectChannelCapacity)
}

object NaiveDelayingAttacker {
  case class AttackParams(autoConnectChannelCapacity: Value)
}
