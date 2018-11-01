package edu.stanford.cs.lnsim.des

/**
  * The environment stores the global state of the simulation and processes all events.
  */
trait Environment {
  type Event

  def initialEvent(): Event
  def processEvent(event: Event): List[(TimeDelta, Event)]
}
