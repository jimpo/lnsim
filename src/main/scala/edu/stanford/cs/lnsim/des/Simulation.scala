package edu.stanford.cs.lnsim.des

import scala.collection.mutable
import org.apache.logging.log4j.LogManager

/**
  * Discrete event simulation. Processes a queue of events in time order, where each event may trigger
  * subsequent events. The simulation runs until a configured end time.
  *
  * @param environment
  * @param endTime Time the simulation ends at naturally
  * @tparam Env
  */
class Simulation[Env <: Environment](private val environment: Env, val endTime: Int) {
  private val eventQueue: mutable.PriorityQueue[(Int, environment.Event)] = new mutable.PriorityQueue()(Ordering.by(_._1))
  private var interrupt: Boolean = false
  private val logger = LogManager.getLogger(classOf[Simulation[Environment]])

  def run(): Unit = {
    eventQueue.enqueue((0, environment.initialEvent()))
    while (!interrupt && eventQueue.nonEmpty) {
      val (time, event) = eventQueue.dequeue()

      // TODO: Working JSON serializer for Events
      logger.info(f"Processing event $event at time $time: {}", event)

      val next = environment.processEvent(event)
      for ((delay, nextEvent) <- next) {
        val nextTime = time + delay
        if (endTime == 0 || nextTime < endTime) {
          eventQueue.enqueue((nextTime, nextEvent))
        }
      }
    }
  }

  def stop(): Unit = interrupt = true
}