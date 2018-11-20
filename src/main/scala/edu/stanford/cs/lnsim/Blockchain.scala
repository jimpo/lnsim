package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta
import edu.stanford.cs.lnsim.log.StructuredLogging

import scala.collection.mutable

import spray.json._
import spray.json.DefaultJsonProtocol._

class Blockchain(val blockInterval: TimeDelta) extends StructuredLogging {
  import Blockchain._

  private var _blockNumber: BlockNumber = 0
  private val onChainEvents: mutable.Map[BlockNumber, List[Notification]] = mutable.TreeMap.empty
  private val fundingTransactions: mutable.Map[ChannelID, BlockNumber] = mutable.HashMap.empty

  def blockArrived(): List[Notification] = {
    val events = onChainEvents.remove(blockNumber).getOrElse(Nil)
    incBlockNumber()

    logger.info(
      "msg" -> "New block arrived".toJson,
      "number" -> blockNumber.toJson,
    )
    events
  }

  def nextBlockTime(): TimeDelta = Util.drawExponential(blockInterval)

  def blockNumber: BlockNumber = _blockNumber
  private def incBlockNumber(): Unit = _blockNumber += 1

  def newFundingTransaction(channelID: ChannelID): Unit = {
    // Assume for now that all funding transactions make it into the next block.
    fundingTransactions(channelID) = blockNumber + 1
  }

  def subscribeChannelConfirmed(nodeID: NodeID, channelID: ChannelID, confirmations: BlockDelta): Boolean = {
    fundingTransactions.get(channelID) match {
      case Some(acceptedHeight) =>
        val confirmedHeight = acceptedHeight + confirmations - 1
        if (confirmedHeight <= blockNumber) {
          return false
        }

        val tail = onChainEvents.getOrElse(confirmedHeight, Nil)
        val notification: Notification = ChannelOpened(channelID, nodeID)
        onChainEvents.put(confirmedHeight, notification :: tail)
        true

      case None =>
        throw new AssertionError(s"Cannot subscribe to unknown channel $channelID")
    }
  }
}

object Blockchain {
  sealed trait Notification
  case class ChannelOpened(channelID: ChannelID, nodeID: NodeID) extends Notification
  case class ChannelClosed(channelID: ChannelID) extends Notification
}
