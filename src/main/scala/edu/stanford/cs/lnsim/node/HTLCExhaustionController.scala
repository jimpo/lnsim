package edu.stanford.cs.lnsim.node

/** HTLCExhaustionController attempts to attack the network by performing loop attacks with long
  * circuits and very small amounts. The attacker seeks to exhaust the number of open accepted
  * HTLC slots.
  */
class HTLCExhaustionController(params: NodeActor.Params,
                               attackParams: AutoPilotCaptureController.AttackParams)
  extends AutoPilotCaptureController(params, attackParams) {

}
