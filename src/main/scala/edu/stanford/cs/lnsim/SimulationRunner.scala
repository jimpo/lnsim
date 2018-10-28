package edu.stanford.cs.lnsim

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

      val env = new Environment(rand = rand)
      val simulation = new Simulation[Environment](env, config.duration)

      Signal.handle(new Signal("INT"), _ => simulation.stop())

      simulation.run()
    case None =>
  }
}
