package edu.stanford.cs.lnsim

import scala.util.Random

object Util {
  def drawExponential(mean: Double, rand: Random): Int = (-mean * math.log(1 - rand.nextDouble())).toInt

  /**
    * Repeat an operation returning a boolean success indicator until it succeeds n times.
    */
  def repeatUntilSuccess(n: Int)(op: => Boolean): Unit = {
    var count = 0
    while (count < n) {
      if (op) {
        count += 1
      }
    }
  }
}
