package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.{Simulation, secondsToTimeDelta}
import edu.stanford.cs.lnsim.spec.SimulationSpec
import sun.misc.Signal

import scala.io.Source
import scala.util.Random

import spray.json._
import JSONProtocol._

object SimulationRunner extends App {
  Config.parser.parse(args, Config()) match {
    case Some(config) =>
      config.randomSeed match {
        case Some(seed) => Random.setSeed(seed)
        case None =>
      }

      val specFileContents = Source.fromFile(config.specFileName.get).getLines.mkString
      val spec = specFileContents.parseJson.convertTo[SimulationSpec]

      val blockchain = new Blockchain(
        blockInterval = secondsToTimeDelta(config.blockInterval),
        feePerWeight = config.feePerWeight,
        rand = new Random(Random.nextLong()),
      )
      val graphBuilder = new EnvBuilder(
        spec = spec,
        blockchain = blockchain
      )
      val env = graphBuilder.build(
        config.numDelayAttackNodes,
        config.numLoopAttackNodes,
        config.disableCriticalChannels
      )
      val simulation = new Simulation[Environment](env, secondsToTimeDelta(config.duration))

      Signal.handle(new Signal("INT"), _ => simulation.stop())

      simulation.run()
    case None =>
  }
}
