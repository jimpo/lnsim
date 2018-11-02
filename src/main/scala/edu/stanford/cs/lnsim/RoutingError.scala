package edu.stanford.cs.lnsim

/**
  * An error returned with a failed payment.
  *
  * See BOLT 3: Returning Errors
  */
sealed trait RoutingError
trait BadOnionError extends RoutingError
trait PermanentError extends RoutingError
trait NodeError extends RoutingError
trait UpdateError extends RoutingError

/**
  * Due to a protocol bug, a node may return an incorrectly encoded error message that
  * makes it impossible for the sender to determine at which hop the payment failed.
  *
  * https://github.com/lightningnetwork/lightning-rfc/issues/332
  */
case object NotDecodable extends RoutingError

case object InvalidRealm extends RoutingError with PermanentError
case object TemporaryNodeError extends RoutingError with NodeError
case object PermanentNodeError extends RoutingError with PermanentError with NodeError
case object RequiredNodeFeatureMissing extends RoutingError with PermanentError with NodeError
case object UnknownNextPeer extends RoutingError with PermanentError
case class AmountBelowMinimum(amount: Value) extends RoutingError with UpdateError
case object UnknownPaymentHash extends RoutingError with PermanentError
case object IncorrectPaymentAmount extends RoutingError with PermanentError
case object FinalExpiryTooSoon extends RoutingError
case object FinalIncorrectExpiry extends RoutingError
case object FinalIncorrectHTLCAmount extends RoutingError
