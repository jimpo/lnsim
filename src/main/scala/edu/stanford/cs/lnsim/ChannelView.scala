package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.Timestamp

import scala.collection.mutable

class ChannelView(private val channel: Channel,
                  private val ourParams: ChannelParams,
                  private val theirParams: ChannelParams) {
  import ChannelView._

  private var ourNextHTLCID = 0
  private var theirNextHTLCID = 0
  private var ourBalance: Value = 0
  private var theirBalance: Value = 0
  private var ourHTLCs: mutable.Map[Int, HTLCDesc] = mutable.HashMap.empty
  private var theirHTLCs: mutable.Map[Int, HTLCDesc] = mutable.HashMap.empty

  def addLocalHTLC(htlcWithoutID: HTLCDesc): Either[HTLCDesc, Error.Value] = {
    if (htlcWithoutID.id != -1) {
      return Right(Error.IncorrectHTLCID)
    }
    checkAddHTLC(htlcWithoutID, ourAvailableBalance, ourHTLCs, ourParams) match {
      case Some(error) => return Right(error)
      case None =>
    }

    val htlc = htlcWithoutID.copy(id = ourNextHTLCID)
    ourNextHTLCID += 1

    ourBalance -= htlc.amount
    ourHTLCs(htlc.id) = htlc

    Left(htlc)
  }

  def addRemoteHTLC(htlc: HTLCDesc): Either[Unit, Error.Value] = {
    if (htlc.id != theirNextHTLCID) {
      return Right(Error.IncorrectHTLCID)
    }
    checkAddHTLC(htlc, theirAvailableBalance, theirHTLCs, ourParams) match {
      case Some(error) => return Right(error)
      case None =>
    }

    theirBalance -= htlc.amount
    theirHTLCs(htlc.id) = htlc

    Left(())
  }

  // TODO: Transaction weight/total fee check
  def ourAvailableBalance: Value = ourBalance - theirParams.requiredReserve
  def theirAvailableBalance: Value = theirBalance - ourParams.requiredReserve

  private def checkAddHTLC(htlc: HTLCDesc,
                           availableBalance: Value,
                           htlcs: mutable.Map[Int, HTLCDesc],
                           channelParams: ChannelParams): Option[Error.Value] = {

    if (htlc.amount < channelParams.htlcMinimum) {
      return Some(Error.BelowHTLCMinimum)
    }
    if (availableBalance < htlc.amount) {
      return Some(Error.InsufficientBalance)
    }
    if (ourHTLCs.size + 1 > channelParams.maxAcceptedHTLCs) {
      return Some(Error.ExceedsMaxAcceptedHTLCs)
    }

    val htlcValueInFlight = htlcs.valuesIterator.foldLeft(0: Value)(_ + _.amount)
    if (htlcValueInFlight + htlc.amount > channelParams.maxHTLCInFlight) {
      return Some(Error.ExceedsMaxHTLCInFlight)
    }

    None
  }
}

object ChannelView {
  case class HTLCDesc(id: Int, amount: Value, expiry: Timestamp, paymentID: PaymentID)

  private sealed trait Update
  private case class UpdateAdd(htlc: HTLC) extends Update
  private case class UpdateFail(id: Int) extends Update
  private case class UpdateFulfill(id: Int) extends Update

  object Error extends Enumeration {
    type Error = Value
    val IncorrectHTLCID, InsufficientBalance, BelowHTLCMinimum, ExceedsMaxHTLCInFlight, ExceedsMaxAcceptedHTLCs = Value
  }
}
