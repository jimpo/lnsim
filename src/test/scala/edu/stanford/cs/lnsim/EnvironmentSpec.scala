package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta
import edu.stanford.cs.lnsim.spec.{NodeSpec, SimulationSpec, TransactionSpec}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers, OneInstancePerTest}

//noinspection ZeroIndexToHead
class EnvironmentSpec extends FunSpec with OneInstancePerTest with BeforeAndAfter with Matchers {
  describe("An Environment") {
    val blockchain = new Blockchain(Config.DefaultBlockInterval, Config.DefaultFeePerWeight)

    describe("when processing NewBlock events") {
      it("registers blocks in order") {
        val spec = SimulationSpec(nodes = List(), transactions = List())
        val env = new EnvBuilder(spec, blockchain).build()

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

    it("starts the simulation by scheduling all transactions and first block") {
      val nodeIDs = (0 to 3).map(_  => Util.randomUUID()).toArray
      val spec = SimulationSpec(
        nodes = nodeIDs.map(NodeSpec(_)).toList,
        transactions = List(
          TransactionSpec(1, nodeIDs(0), nodeIDs(1), 10000, Util.randomUUID()),
          TransactionSpec(5, nodeIDs(1), nodeIDs(2), 20000, Util.randomUUID()),
        ),
      )
      val env = new EnvBuilder(spec, blockchain).build()

      var newEvents: List[(TimeDelta, env.Event)] = Nil
      val scheduleEvent = (delay: TimeDelta, newEvent: env.Event) =>
        newEvents = (delay, newEvent) :: newEvents
      env.processEvent(env.initialEvent(), 0, scheduleEvent)

      assert(newEvents.length == 3)

      val Some((_, events.NewBlock(blockNumber))) =
        newEvents.find(_._2.isInstanceOf[events.NewBlock])
      assert(blockNumber == 1)

      val Some((payment1Time, events.NewPayment(payment1))) =
        newEvents.find(scheduledEvent =>
          scheduledEvent._2.isInstanceOf[events.NewPayment] &&
          scheduledEvent._2.asInstanceOf[events.NewPayment].paymentInfo.paymentID ==
            spec.transactions(0).paymentID
        )
      assert(payment1Time == spec.transactions(0).timestamp)
      assert(payment1.amount == spec.transactions(0).amount)
      assert(payment1.sender.id == spec.transactions(0).sender)
      assert(payment1.recipientID == spec.transactions(0).recipient)

      val Some((payment2Time, events.NewPayment(payment2))) =
        newEvents.find(scheduledEvent =>
          scheduledEvent._2.isInstanceOf[events.NewPayment] &&
            scheduledEvent._2.asInstanceOf[events.NewPayment].paymentInfo.paymentID ==
              spec.transactions(1).paymentID
        )
      assert(payment2Time == spec.transactions(1).timestamp)
      assert(payment2.amount == spec.transactions(1).amount)
      assert(payment2.sender.id == spec.transactions(1).sender)
      assert(payment2.recipientID == spec.transactions(1).recipient)
    }

    it("schedules delivery of messages sent between nodes") {
      val nodeIDs = (0 to 3).map(_ => Util.randomUUID()).toArray
      val spec = SimulationSpec(
        nodes = nodeIDs.map(NodeSpec(_)).toList,
        transactions = List(),
      )
      val env = new EnvBuilder(spec, blockchain).build()

      var newEvents: List[(TimeDelta, env.Event)] = Nil
      val scheduleEvent = (delay: TimeDelta, newEvent: env.Event) =>
        newEvents = (delay, newEvent) :: newEvents

      val payment = PaymentInfo(
        sender = env.node(nodeIDs(0)).get,
        recipientID = nodeIDs(1),
        amount = 10000,
        finalExpiryDelta = 144,
        paymentID = Util.randomUUID(),
      )
      env.processEvent(events.NewPayment(payment), 1, scheduleEvent)

      assert(newEvents.length == 1)
      var (eventTime, event) = newEvents.head
      newEvents = Nil

      assert(event.isInstanceOf[events.ReceiveMessage])
      val events.ReceiveMessage(sender, recipient, message) = event
      assert(sender.id == payment.sender.id)
      assert(recipient.id == payment.recipientID)
      assert(message.isInstanceOf[OpenChannel])
    }
  }
}
