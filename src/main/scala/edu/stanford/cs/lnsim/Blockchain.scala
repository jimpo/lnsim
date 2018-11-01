package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

import scala.util.Random

class Blockchain(val blockInterval: TimeDelta, val rand: Random) {
  def nextBlockTime(): TimeDelta = Util.drawExponential(blockInterval, rand)
}
