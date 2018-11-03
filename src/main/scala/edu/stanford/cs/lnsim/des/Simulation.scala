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
class Simulation[Env <: Environment](private val environment: Env, val endTime: Timestamp) {
  private val eventQueue: mutable.PriorityQueue[(Timestamp, environment.Event)] = new mutable.PriorityQueue()(Ordering.by(_._1))
  private var interrupt: Boolean = false
  private val logger = LogManager.getLogger(classOf[Simulation[Environment]])
  private var currentTime: Timestamp = 0

  def run(): Unit = {
    scheduleEvent(0, environment.initialEvent())
    while (!interrupt && eventQueue.nonEmpty) {
      val (newTime, event) = eventQueue.dequeue()

      // TODO: Working JSON serializer for Events
      logger.info(f"Processing event $event at time $newTime: {}", event)

      currentTime = newTime
      environment.processEvent(event, scheduleEvent)
    }
  }

  private def scheduleEvent(delay: TimeDelta, event: environment.Event): Unit =
    eventQueue.enqueue((currentTime + delay, event))

  def stop(): Unit = interrupt = true
}