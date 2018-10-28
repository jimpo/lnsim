package edu.stanford.cs.lnsim

import scala.util.Random

class Blockchain(val blockInterval: Int, val rand: Random) {
  def nextBlockTime(): Int = drawExponential(blockInterval)
  def drawExponential(mean: Double): Int = (-mean * math.log(1 - rand.nextDouble())).toInt
}
