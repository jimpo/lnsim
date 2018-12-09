package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.log.StructuredLogging

/** A NaiveDelayingController implements an opportunistic attacker strategy where it will reduce
  * fees and delay any HTLCs forwarded through it for the maximum amount of time before the HTLC
  * expires without forcing a unilateral on-chain close.
 */
class NaiveDelayingController(params: NodeActor.Params,
                              attackParams: AutoPilotCaptureController.AttackParams)
  extends AutoPilotCaptureController(params, attackParams) with StructuredLogging {

  override def forwardHTLC(prevHop: HTLC, nextHop: HTLC, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber]) = {
    super.forwardHTLC(prevHop, nextHop, blockNumber) match {
      case ret @ (Some(_error), _) => ret
      case (None, _) => (Some(TemporaryChannelFailure), Some(prevHop.expiry - params.minExpiry))
    }
  }
}
