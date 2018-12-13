package edu.stanford.cs.lnsim

import java.util.UUID

import spray.json.{JsField, JsObject}

import scala.util.Random

object Util {
  def drawExponential(mean: Double, rand: Random = null): Int = {
    val randDouble = if (rand == null) Random.nextDouble() else rand.nextDouble()
    (-mean * math.log(1 - randDouble)).toInt
  }

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

  /** Efficiently sample from a weighted distribution over values given the cumulative mass
    * function (CMF). The CMF is given as an array of values and their associated cumulative
    * weights, which must be in increasing order. The function runs in time O(log n) in the number
    * of values.
    */
  def sampleCMF[T](cmf: Array[(T, Int)]): T = {
    val (_, totalWeight) = cmf.last
    var index = Random.nextInt(totalWeight)

    var lo = 0
    var hi = cmf.length - 1
    while (lo != hi) {
      val mid = (lo + hi) / 2
      val (_, weight) = cmf(mid)
      if (index <= weight) {
        hi = mid
      } else {
        lo = mid + 1
      }
    }

    cmf(lo)._1
  }

  def extendJsObject(obj: JsObject, fields: JsField*): JsObject =
    JsObject((obj.fields.toSeq ++ fields): _*)
}
