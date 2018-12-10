package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.graph.NetworkGraphView

trait NodeController {
  /** Decide whether to forward an HTLC or reject it.
    *
    * @return The first element is Some(error) if the HTLC should be failed and None if it is to
    *     be forwarded. The second element is Some(BlockNumber) if the response should be delayed
    *     until the specified block or None if it is to be sent immediately.
    */
  def forwardHTLC(prevHop: HTLC, nextHop: HTLC, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber])

  /** Decide whether to accept an HTLC terminating at this node or reject it.
    *
    * @return The first element is Some(error) if the HTLC should be failed and None if it is to
    *     be forwarded. The second element is Some(BlockNumber) if the response should be delayed
    *     until the specified block or None if it is to be sent immediately.
    */
  def acceptHTLC(htlc: HTLC, finalHop: FinalHop, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber])

  /** Decide which new channels to open. The total capacity of the channels requested must not
    * exceed the budget given as a parameter.
    *
    * @param sourceNodeID The ID of the node initiating the new channels.
    * @param budget The maximum allowed capacity of the channels requested.
    * @param graphView A reference to the node's view of the network graph.
    * @return A list of channels to open, specifying the node and capacity of each
    */
  def autoConnect(sourceNodeID: NodeID,
                  budget: Value,
                  graphView: NetworkGraphView): Seq[(NodeID, Value)]

  def bootstrapEndActions(): Seq[NodeAction]
}
