package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.routing.{NetworkGraphView, Router}

import spray.json._
import spray.json.DefaultJsonProtocol._
import JSONProtocol._

/** DelayingActor implements an adversarial node that will withhold any HTLCs routed through it for
  * the maximum amount of time before failing them. If it is the final hop in a route, the node
  * will handle the HTLC normally and respond immediately.
  */
class DelayingActor(id: NodeID,
                    params: NodeActor.Params,
                    attackParams: DelayingController.AttackParams,
                    controller: NodeController,
                    output: ObservableOutput,
                    router: Router,
                    graphView: NetworkGraphView,
                    blockchain: BlockchainView)
  extends NodeActor(id, params, controller, output, router, graphView, blockchain) {

  override def handleBootstrapEnd()(implicit ctx: NodeContext): Unit = {
    logger.debug(
      "msg" -> "Attacker activating".toJson,
      "nodeID" -> id.toJson,
    )
    openNewChannels(attackParams.numChannels * attackParams.channelCapacity)
  }
}

