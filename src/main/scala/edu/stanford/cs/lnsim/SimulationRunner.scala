package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{Simulation, secondsToTimeDelta}
import sun.misc.Signal

import scala.util.Random

object SimulationRunner extends App {
  Config.parser.parse(args, Config()) match {
    case Some(config) =>
      val rand = config.randomSeed match {
        case Some(seed) => new Random(seed)
        case None => new Random()
      }

      val blockchain = new Blockchain(
        blockInterval = secondsToTimeDelta(config.blockInterval),
        rand = rand
      )
      val graphBuilder = new RandomGraphBuilder(rand, numNodes = 100, avgChannelsPerNode = 2)
      val env = new Environment(
        rand = rand,
        blockchain = blockchain,
        graphBuilder = graphBuilder
      )
      val simulation = new Simulation[Environment](env, secondsToTimeDelta(config.duration))

      Signal.handle(new Signal("INT"), _ => simulation.stop())

      simulation.run()
    case None =>
  }
}
