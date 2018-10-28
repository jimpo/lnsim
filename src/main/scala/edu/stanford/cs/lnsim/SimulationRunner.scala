package edu.stanford.cs.lnsim

import java.util

import edu.stanford.cs.lnsim.des.Simulation
import sun.misc.Signal

import scala.util.Random

object SimulationRunner extends App {
  Config.parser.parse(args, Config()) match {
    case Some(config) =>
      val rand = config.randomSeed match {
        case Some(seed) => new Random(seed)
        case None => new Random()
      }

      val networkGraph = new NetworkGraph(util.ArrayList.empty)
      val blockchain = new Blockchain(blockInterval = config.blockInterval, rand = rand)
      val env = new Environment(rand = rand, blockchain = blockchain, networkGraph = networkGraph)
      val simulation = new Simulation[Environment](env, config.duration)

      Signal.handle(new Signal("INT"), _ => simulation.stop())

      simulation.run()
    case None =>
  }
}
