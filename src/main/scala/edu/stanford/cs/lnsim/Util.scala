package edu.stanford.cs.lnsim

import scala.util.Random

object Util {
  def drawExponential(mean: Double, rand: Random): Int = (-mean * math.log(1 - rand.nextDouble())).toInt
}
