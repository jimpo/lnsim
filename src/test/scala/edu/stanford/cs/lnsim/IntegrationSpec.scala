package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{Simulation, Timestamp, secondsToTimeDelta}
import edu.stanford.cs.lnsim.spec.{NodeSpec, SimulationSpec, TransactionSpec}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers, OneInstancePerTest}
import org.scalamock.scalatest.MockFactory

class IntegrationSpec extends FunSpec
  with OneInstancePerTest with BeforeAndAfter with Matchers with MockFactory {

  describe("End-to-end simulation tests") {
    val blockchain = new Blockchain(
      secondsToTimeDelta(Config.DefaultBlockInterval),
      Config.DefaultFeePerWeight
    )

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
      val paymentIDs = spec.transactions.map(_.paymentID)

      val output = mock[ObservableOutput]
      val env = new EnvBuilder(output, spec, blockchain).build()
      val simulation = new Simulation[Environment](env, spec.endTime)

      (output.openChannel _).expects(
        *, nodeIDs(0), nodeIDs(1), EclairDefaults.MinFundingAmount, *, Some(paymentIDs(0))
      )
      (output.paymentCompleted _).expects(
        where { (payment: PendingPayment, _timestamp: Timestamp) =>
          payment.info.paymentID == paymentIDs(0) && payment.route.isEmpty
        }
      )

      simulation.run()
    }

    it("delivering a payment through a two-hop route") {
      val nodeIDs = (1 to 3).map(_ => Util.randomUUID()).toArray
      val amount = 10000000000L
      val spec = SimulationSpec(
        nodes = nodeIDs.map(NodeSpec(_)).toList,
        transactions = List(
          TransactionSpec(1, nodeIDs(0), nodeIDs(1), amount, Util.randomUUID()),
          TransactionSpec(
            secondsToTimeDelta(2 * 60 * 60), nodeIDs(1), nodeIDs(2), amount, Util.randomUUID()
          ),
          TransactionSpec(
            secondsToTimeDelta(4 * 60 * 60), nodeIDs(0), nodeIDs(2), amount / 2, Util.randomUUID()
          ),
          TransactionSpec(
            secondsToTimeDelta(6 * 60 * 60), nodeIDs(2), nodeIDs(0), amount / 2, Util.randomUUID()
          ),
        ),
        channelBudgets = List(),
        startTime = 0,
        endTime = secondsToTimeDelta(24 * 60 * 60),
      )
      val paymentIDs = spec.transactions.map(_.paymentID)

      val output = mock[ObservableOutput]
      val env = new EnvBuilder(output, spec, blockchain).build()
      val simulation = new Simulation[Environment](env, spec.endTime)

      (output.openChannel _).expects(
        *, nodeIDs(0), nodeIDs(1), 2 * amount, *, Some(paymentIDs(0))
      )
      (output.openChannel _).expects(
        *, nodeIDs(1), nodeIDs(2), 2 * amount, *, Some(paymentIDs(1))
      )
      (output.paymentCompleted _).expects(
        where { (payment: PendingPayment, _timestamp: Timestamp) =>
          payment.info.paymentID == paymentIDs(0) && payment.route.isEmpty
        }
      )
      (output.paymentCompleted _).expects(
        where { (payment: PendingPayment, _timestamp: Timestamp) =>
          payment.info.paymentID == paymentIDs(1) && payment.route.isEmpty
        }
      )
      (output.paymentCompleted _).expects(
        where { (payment: PendingPayment, _timestamp: Timestamp) =>
          payment.info.paymentID == paymentIDs(2) && payment.route.length == 2
        }
      )
      (output.paymentCompleted _).expects(
        where { (payment: PendingPayment, _timestamp: Timestamp) =>
          payment.info.paymentID == paymentIDs(3) && payment.route.length == 2
        }
      )

      simulation.run()
    }
  }
}
