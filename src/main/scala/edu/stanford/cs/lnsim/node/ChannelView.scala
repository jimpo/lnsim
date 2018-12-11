package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._

import scala.collection.mutable

/** A ChannelView is a node's view of a local channel with another node. This class manages
  * balances and HTLCs.
  */
class ChannelView(val otherNode: NodeID,
                  val ourInitialBalance: Value,
                  val theirInitialBalance: Value,
                  val ourParams: ChannelParams,
                  val theirParams: ChannelParams) {
  import ChannelView._

  private var status: Status.Value = Status.Opening
  private val ourSide: Side = new Side(ourInitialBalance)
  private val theirSide: Side = new Side(theirInitialBalance)

  def transition(newStatus: Status.Value): Unit = {
    (status, newStatus) match {
      case (Status.Opening, Status.Active) |
           (Status.Active, Status.Disabled) |
           (Status.Disabled, Status.Active) |
           (_, Status.Closing) =>
        status = newStatus

      case _ =>
        throw new AssertionError(s"Channel cannot transition from $status to $newStatus")
    }
  }

  def addLocalHTLC(htlc: HTLC.Desc): Option[Error.Value] = status match {
    case Status.Active => ourSide.addHTLC(htlc, theirParams)
    case _ => Some(Error.Inactive)
  }
  def addRemoteHTLC(htlc: HTLC.Desc): Option[Error.Value] =
    theirSide.addHTLC(htlc, ourParams)

  def failLocalHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] =
    ourSide.removeHTLC(id) match {
      case Some(htlc) =>
        ourSide.balance += htlc.amount
        Right(htlc)
      case None => Left(Error.IncorrectHTLCID)
    }
  def failRemoteHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] =
    theirSide.removeHTLC(id) match {
      case Some(htlc) =>
        theirSide.balance += htlc.amount
        Right(htlc)
      case None => Left(Error.IncorrectHTLCID)
    }

  def fulfillLocalHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] =
    ourSide.removeHTLC(id) match {
      case Some(htlc) =>
        theirSide.balance += htlc.amount
        Right(htlc)
      case None => Left(Error.IncorrectHTLCID)
    }
  def fulfillRemoteHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] =
    theirSide.removeHTLC(id) match {
      case Some(htlc) =>
        ourSide.balance += htlc.amount
        Right(htlc)
      case None => Left(Error.IncorrectHTLCID)
    }

  def ourNextHTLCID: HTLCID = ourSide.nextHTLCID

  def ourAvailableBalance: Value = ourSide.availableBalance(theirParams)
  def theirAvailableBalance: Value = theirSide.availableBalance(ourParams)

  def ourAvailableHTLCs: Value = ourSide.availableHTLCs(theirParams)
  def theirAvailableHTLCs: Value = theirSide.availableHTLCs(ourParams)

  def isClosing: Boolean = status == Status.Closing
  def isClosed: Boolean = isClosing && ourSide.htlcsEmpty && theirSide.htlcsEmpty
}

object ChannelView {
  private sealed trait Update
  private case class UpdateAdd(htlc: HTLC) extends Update
  private case class UpdateFail(id: Int) extends Update
  private case class UpdateFulfill(id: Int) extends Update

  class Side(initialBalance: Value) {
    var balance: Value = initialBalance
    private var _nextHTLCID: HTLCID = 0
    private val htlcs: mutable.Map[HTLCID, HTLC.Desc] = mutable.Map.empty

    def addHTLC(htlc: HTLC.Desc, otherParams: ChannelParams): Option[Error.Value] = {
      if (htlc.id != nextHTLCID) {
        return Some(Error.IncorrectHTLCID)
      }
      if (htlc.amount < otherParams.htlcMinimum) {
        return Some(Error.BelowHTLCMinimum)
      }
      if (availableBalance(otherParams) < htlc.amount) {
        return Some(Error.InsufficientBalance)
      }
      if (htlcs.size + 1 > otherParams.maxAcceptedHTLCs) {
        return Some(Error.ExceedsMaxAcceptedHTLCs)
      }

      val htlcValueInFlight = htlcs.valuesIterator.foldLeft(0: Value)(_ + _.amount)
      if (htlcValueInFlight + htlc.amount > otherParams.maxHTLCInFlight) {
        return Some(Error.ExceedsMaxHTLCInFlight)
      }

      incHTLCID()
      balance -= htlc.amount
      htlcs(htlc.id) = htlc

      None
    }

    def removeHTLC(id: HTLCID): Option[HTLC.Desc] = htlcs.remove(id)

    // TODO: Transaction weight/total fee check
    def availableBalance(otherParams: ChannelParams): Value = balance - otherParams.requiredReserve

    def availableHTLCs(otherParams: ChannelParams): Int = otherParams.maxAcceptedHTLCs - htlcs.size

    def htlcsEmpty: Boolean = htlcs.isEmpty

    def nextHTLCID: HTLCID = _nextHTLCID
    private def incHTLCID(): Unit = _nextHTLCID += 1
  }

  object Error extends Enumeration {
    type Error = Value
    val IncorrectHTLCID, InsufficientBalance, BelowHTLCMinimum, ExceedsMaxHTLCInFlight,
    ExceedsMaxAcceptedHTLCs, Inactive = Value
  }

  object Status extends Enumeration {
    type Error = Value

    /** Opening/Closing/Disabled: No new local HTLCs may be added.
      * Active: New HTLCs may be added.
      */
    val Opening, Active, Disabled, Closing = Value
  }
}
