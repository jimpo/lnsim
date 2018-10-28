package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.Simulation
import sun.misc.Signal

object SimulationRunner extends App {
  val env = new Environment()
  val simulation = new Simulation[Environment](env, 1)

  Signal.handle(new Signal("INT"), _ => simulation.stop())

  simulation.run()
}
