package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{Simulation, secondsToTimeDelta}
import edu.stanford.cs.lnsim.spec.{NodeSpec, SimulationSpec, TransactionSpec}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers, OneInstancePerTest}

class IntegrationSpec extends FunSpec with OneInstancePerTest with BeforeAndAfter with Matchers {
  describe("End-to-end simulation tests") {
    val blockchain = new Blockchain(Config.DefaultBlockInterval, Config.DefaultFeePerWeight)

    it("delivering a single payment by opening a new channel") {
      val nodeIDs = (0 until 2).map(_  => Util.randomUUID()).toArray
      val spec = SimulationSpec(
        nodes = nodeIDs.map(NodeSpec(_)).toList,
        transactions = List(
          TransactionSpec(1, nodeIDs(0), nodeIDs(1), 10000, Util.randomUUID()),
        ),
        channelBudgets = List(),
        startTime = 0,
        endTime = secondsToTimeDelta(24 * 60 * 60),
      )
      val env = new EnvBuilder(spec, blockchain).build()
      val simulation = new Simulation[Environment](env, spec.endTime)

      simulation.run()
    }
  }
}
