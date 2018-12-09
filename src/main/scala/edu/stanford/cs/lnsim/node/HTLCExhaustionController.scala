package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._

/** HTLCExhaustionController attempts to attack the network by performing loop attacks with long
  * circuits and very small amounts. The attacker seeks to exhaust the number of open accepted
  * HTLC slots.
  */
class HTLCExhaustionController(params: NodeActor.Params,
                               attackParams: AutoPilotCaptureController.AttackParams,
                               attackerNodeIDs: Set[NodeID])
  extends AutoPilotCaptureController(params, attackParams) {

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
}
