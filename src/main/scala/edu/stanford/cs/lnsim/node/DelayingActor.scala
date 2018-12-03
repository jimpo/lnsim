package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.routing.{NetworkGraphView, Router}

/** DelayingActor implements an adversarial node that will withhold any HTLCs routed through it for
  * the maximum amount of time before failing them. If it is the final hop in a route, the node
  * will handle the HTLC normally and respond immediately.
  */
class DelayingActor(id: NodeID,
                    params: NodeActor.Params,
                    output: ObservableOutput,
                    router: Router,
                    graphView: NetworkGraphView,
                    blockchain: BlockchainView)
  extends NodeActor(id, params, output, router, graphView, blockchain) {

  import DelayingActor._

  override def handleUpdateAddHTLC(sender: NodeID, message: UpdateAddHTLC)
                                  (implicit ctx: NodeContext): Unit = {
    val UpdateAddHTLC(route) = message
    val (hop :: nextHops) = route.hops

    addRemoteHTLC(hop)

    val backwardsRoute = BackwardRoutingPacket((hop.channel, hop.id) :: route.backwardRoute.hops)
    if (nextHops.isEmpty) {
      processFinalHopHTLC(hop, route.finalHop, backwardsRoute)
    } else {
      val releaseNumber = hop.expiry - ReleaseHTLCDelta - blockchain.blockNumber
      val newRoute = ForwardRoutingPacket(nextHops, route.finalHop, backwardsRoute)

      if (!blockchain.subscribeAction(releaseNumber, FailForwardHTLC(newRoute))) {
        // If we failed to register the subscription, just forward it normally.
        processIntermediateHopHTLC(newRoute)
      }
    }
  }

  override def handleAction(action: NodeAction)(implicit ctx: NodeContext): Unit = action match {
    case FailForwardHTLC(route) => failForwardHTLC(route)
    case _ => super.handleAction(action)
  }

  private def failForwardHTLC(route: ForwardRoutingPacket)
                             (implicit ctx: NodeContext): Unit = {
    val nextHop = route.hops.head
    val prevHop = route.backwardRoute.hops.head
    val prevChannelID = prevHop._1.id
    val prevHTLCID = prevHop._2

    val prevChannel = lookupChannel(prevChannelID)
      .getOrElse(throw new HTLCUpdateFailure(
        s"Node $id received HTLC on unknown channel $prevChannelID"))

    prevChannel.failRemoteHTLC(prevHTLCID).left.foreach(error => throw new HTLCUpdateFailure(
      s"Error failing newly added HTLC $prevHTLCID on channel $prevChannelID: $error"
    ))

    val error = TemporaryChannelFailure
    val failMsg = UpdateFailHTLC(route.backwardRoute, error, Some(nextHop.channel))
    ctx.sendMessage(prevChannel.otherNode, failMsg)
  }
}

object DelayingActor {
  // Number of blocks before HTLC expiry when HTLCs that are withheld are released.
  val ReleaseHTLCDelta: BlockDelta = 20
}
