package edu.stanford.cs.lnsim.node
import edu.stanford.cs.lnsim
import edu.stanford.cs.lnsim._
import edu.stanford.cs.lnsim.routing.NetworkGraphView

class DefaultController(private val feeBase: Value,
                        private val feeProportionalMillionths: Long,
                        private val finalExpiryDelta: BlockDelta,
                        private val requiredExpiryDelta: BlockDelta,
                        private val minExpiry: BlockDelta,
                        private val maxExpiry: BlockDelta) extends NodeController {

  override def forwardHTLC(prevHop: lnsim.HTLC, nextHop: lnsim.HTLC, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber]) = {
    if (nextHop.expiry - prevHop.expiry < requiredExpiryDelta) {
      return (Some(IncorrectExpiryDelta), None)
    }
    if (nextHop.expiry - blockNumber < minExpiry) {
      return (Some(ExpiryTooSoon), None)
    }
    if (nextHop.expiry - blockNumber > maxExpiry) {
      return (Some(ExpiryTooFar), None)
    }

    val requiredFee = feeBase + nextHop.amount * feeProportionalMillionths
    if (nextHop.amount - prevHop.amount < requiredFee) {
      return (Some(FeeInsufficient), None)
    }

    (None, None)
  }

  override def acceptHTLC(htlc: HTLC, finalHop: FinalHop, blockNumber: BlockNumber)
  : (Option[RoutingError], Option[BlockNumber]) = {
    if (!finalHop.paymentIDKnown) {
      return (Some(UnknownPaymentHash), None)
    }
    if (htlc.amount < finalHop.amount) {
      return (Some(FinalIncorrectHTLCAmount(htlc.amount)), None)
    }
    if (htlc.expiry < finalHop.expiry) {
      return (Some(FinalIncorrectExpiry(htlc.expiry)), None)
    }
    if (finalHop.expiry < blockNumber + finalExpiryDelta) {
      return (Some(FinalExpiryTooSoon), None)
    }

    (None, None)
  }

  override def autoConnect(sourceNodeID: NodeID,
                           budget: Value,
                           graphView: NetworkGraphView): Seq[(NodeID, Value)] =
    Seq()
}
