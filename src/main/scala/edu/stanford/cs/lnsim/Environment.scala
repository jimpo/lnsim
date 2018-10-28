package edu.stanford.cs.lnsim

import scala.util.Random

class Environment(private val rand: Random) extends des.Environment {
  override type Event = events.Base

  override def initialEvent(): Event = events.Start

  override def processEvent(event: Event): List[(Int, Event)] = event match {
    case events.Start => List.empty
  }
}
