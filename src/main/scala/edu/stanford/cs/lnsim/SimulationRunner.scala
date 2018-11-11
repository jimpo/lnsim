package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{Simulation, secondsToTimeDelta}
import sun.misc.Signal

import scala.util.Random

object SimulationRunner extends App {
  Config.parser.parse(args, Config()) match {
    case Some(config) =>
      config.randomSeed match {
        case Some(seed) => Random.setSeed(seed)
        case None =>
      }

      val blockchain = new Blockchain(
        blockInterval = secondsToTimeDelta(config.blockInterval)
      )
      val graphBuilder = new RandomGraphBuilder(numNodes = 100, avgChannelsPerNode = 2, blockchain = blockchain)
      val env = new Environment(
        blockchain = blockchain,
        graphBuilder = graphBuilder
      )
      val simulation = new Simulation[Environment](env, secondsToTimeDelta(config.duration))

      Signal.handle(new Signal("INT"), _ => simulation.stop())

      simulation.run()
    case None =>
  }
}
