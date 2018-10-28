package edu.stanford.cs.lnsim

import scala.util.Random

class Blockchain(val blockInterval: Int, val rand: Random) {
  def nextBlockTime(): Int = Util.drawExponential(blockInterval, rand)
}
