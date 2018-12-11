package edu.stanford.cs.lnsim

import scala.util.Random

/** IncrementalSampler provides an efficient, immutable interface for computing an iteratively
  * updating random sample. With each new item, the output sampled element is updated to the
  * latest one proportional to its weight.
  *
  * Example:
  *
  * val itemsWithWeights: Seq[Thingamajig, Int] = ???
  * val randomItem = itemsWithWeights.foldLeft(new IncrementalSampler[Thingamajig]) {
  *   (sampler, itemWithWeight) => sampler.update(itemWithWeight._1, itemWithWeight._2)
  * }.get
  */
class IncrementalSampler[T] private (private val rand: Random,
                                     val value: Option[T],
                                     private val cumulativeWeight: Int) {

  def this(seed: Long) = this(new Random(seed), None, 0)

  def update(element: T, weight: Int): IncrementalSampler[T] = {
    if (weight == 0) {
      this
    } else {
      val nextWeight = cumulativeWeight + weight
      val nextValue = if (rand.nextInt(nextWeight) < weight) Some(element) else value
      new IncrementalSampler(rand, nextValue, nextWeight)
    }
  }
}
