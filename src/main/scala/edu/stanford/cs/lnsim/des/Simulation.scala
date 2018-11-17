package edu.stanford.cs.lnsim.des

import edu.stanford.cs.lnsim.log.StructuredLogging

import scala.collection.mutable
import spray.json._
import spray.json.DefaultJsonProtocol._

/**
  * Discrete event simulation. Processes a queue of events in time order, where each event may trigger
  * subsequent events. The simulation runs until a configured end time.
  *
  * @param environment
  * @param endTime Time the simulation ends at naturally
  * @tparam Env
  */
class Simulation[Env <: Environment](private val environment: Env, val endTime: Timestamp)
  extends StructuredLogging {

  private val eventQueue: mutable.PriorityQueue[(Timestamp, environment.Event)] =
    new mutable.PriorityQueue()(Ordering.by(-_._1))
  private var interrupt: Boolean = false
  private var currentTime: Timestamp = 0

  def run(): Unit = {
    scheduleEvent(0, environment.initialEvent())
    while (!interrupt && eventQueue.nonEmpty) {
      val (newTime, event) = eventQueue.dequeue()

      // TODO: Working JSON serializer for Events
      logger.debug(
        "message" -> "Processing event".toJson,
        "time" -> newTime.toString.toJson,
        "event" -> event.toString.toJson
      )

      currentTime = newTime
      environment.processEvent(event, currentTime, scheduleEvent)
    }
  }

  private def scheduleEvent(delay: TimeDelta, event: environment.Event): Unit = {
    if (currentTime + delay < endTime) {
      eventQueue.enqueue((currentTime + delay, event))
    }
  }

  def stop(): Unit = interrupt = true
}