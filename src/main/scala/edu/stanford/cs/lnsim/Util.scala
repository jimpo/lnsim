package edu.stanford.cs.lnsim

import java.util.UUID

import scala.util.Random

object Util {
  def drawExponential(mean: Double): Int = (-mean * math.log(1 - Random.nextDouble())).toInt

  /**
    * Generate a random UUID using the Scala global Random object.
    */
  def randomUUID(): UUID = new UUID(Random.nextLong(), Random.nextLong())

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
