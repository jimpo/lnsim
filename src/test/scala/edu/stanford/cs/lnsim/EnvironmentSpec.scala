package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers, OneInstancePerTest}

class EnvironmentSpec extends FunSpec with OneInstancePerTest with BeforeAndAfter with Matchers {
  describe("An Environment") {
    val blockchain = new Blockchain(Config.DefaultBlockInterval, Config.DefaultFeePerWeight)
    val randomGraphBuilder = new GraphBuilder(10, 1, blockchain)
    val env = new Environment(randomGraphBuilder.build(), blockchain)

    describe("when processing NewBlock events") {
      it("registers blocks in order") {
        var newEvents: List[(TimeDelta, env.Event)] = Nil
        val scheduleEvent = (delay: TimeDelta, newEvent: env.Event) =>
          newEvents = (delay, newEvent) :: newEvents
        env.processEvent(events.NewBlock(1), 10, scheduleEvent)

        assert(newEvents.length == 1)
        assert(newEvents(0)._2 == events.NewBlock(2))

        // Process next block.
        newEvents = Nil
        env.processEvent(events.NewBlock(2), 20, scheduleEvent)

        assert(newEvents.length == 1)
        assert(newEvents(0)._2 == events.NewBlock(3))

        // Attempting to process out-of-order block should fail
        newEvents = Nil
        assertThrows[AssertionError] {
          env.processEvent(events.NewBlock(4), 40, scheduleEvent)
        }
      }
    }
  }
}
