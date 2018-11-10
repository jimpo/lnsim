package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.util.Random

class Blockchain(val blockInterval: TimeDelta) {
  private var _blockNumber: BlockNumber = 0

  def blockArrived(): Unit = incBlockNumber()
  def nextBlockTime(): TimeDelta = Util.drawExponential(blockInterval)

  def blockNumber: BlockNumber = _blockNumber
  private def incBlockNumber(): Unit = _blockNumber += 1
}
