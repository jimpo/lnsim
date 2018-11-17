package edu.stanford.cs.lnsim

import scala.collection.mutable

class ChannelView(val otherNode: NodeID,
                  val ourInitialBalance: Value,
                  val theirInitialBalance: Value,
                  val ourParams: ChannelParams,
                  val theirParams: ChannelParams) {
  import ChannelView._

  private var status: Status.Value = Status.Opening
  private val ourState: State = new State(ourInitialBalance)
  private val theirState: State = new State(theirInitialBalance)

  def transition(newStatus: Status.Value): Unit = {
    (status, newStatus) match {
      case (Status.Opening, Status.Active) |
           (Status.Active, Status.Closing) =>
        status = newStatus

      case _ =>
        throw new AssertionError(s"Channel cannot transition from $status to $newStatus")
    }
  }

  def addLocalHTLC(htlc: HTLC.Desc): Option[Error.Value] =
    ourState.addHTLC(htlc, theirParams)
  def addRemoteHTLC(htlc: HTLC.Desc): Option[Error.Value] =
    theirState.addHTLC(htlc, ourParams)

  def failLocalHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] = ourState.removeHTLC(id) match {
    case Some(htlc) =>
      ourState.balance += htlc.amount
      Right(htlc)
    case None => Left(Error.IncorrectHTLCID)
  }
  def failRemoteHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] = theirState.removeHTLC(id) match {
    case Some(htlc) =>
      theirState.balance += htlc.amount
      Right(htlc)
    case None => Left(Error.IncorrectHTLCID)
  }

  def fulfillLocalHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] = ourState.removeHTLC(id) match {
    case Some(htlc) =>
      theirState.balance += htlc.amount
      Right(htlc)
    case None => Left(Error.IncorrectHTLCID)
  }
  def fulfillRemoteHTLC(id: HTLCID): Either[Error.Value, HTLC.Desc] = theirState.removeHTLC(id) match {
    case Some(htlc) =>
      ourState.balance += htlc.amount
      Right(htlc)
    case None => Left(Error.IncorrectHTLCID)
  }

  def ourNextHTLCID: HTLCID = ourState.nextHTLCID

  def ourAvailableBalance: Value = ourState.availableBalance(theirParams)
  def theirAvailableBalance: Value = theirState.availableBalance(ourParams)
}

object ChannelView {
  private sealed trait Update
  private case class UpdateAdd(htlc: HTLC) extends Update
  private case class UpdateFail(id: Int) extends Update
  private case class UpdateFulfill(id: Int) extends Update

  class State(initialBalance: Value) {
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

    def nextHTLCID: HTLCID = _nextHTLCID
    private def incHTLCID(): Unit = _nextHTLCID += 1
  }

  object Error extends Enumeration {
    type Error = Value
    val IncorrectHTLCID, InsufficientBalance, BelowHTLCMinimum, ExceedsMaxHTLCInFlight, ExceedsMaxAcceptedHTLCs = Value
  }

  object Status extends Enumeration {
    type Error = Value
    val Opening, Active, Disabled, Closing = Value
  }
}
