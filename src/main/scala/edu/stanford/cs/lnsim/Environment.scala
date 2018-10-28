package edu.stanford.cs.lnsim

import scala.util.Random

class Environment(private val rand: Random, val blockchain: Blockchain) extends des.Environment {
  override type Event = events.Base

  override def initialEvent(): Event = events.Start

  override def processEvent(event: Event): List[(Int, Event)] = event match {
    case events.Start => List((blockchain.nextBlockTime(), events.NewBlock(0)))
    case events.NewBlock(number) => List((blockchain.nextBlockTime(), events.NewBlock(number + 1)))
  }
}
