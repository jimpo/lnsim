package edu.stanford.cs.lnsim

import scopt.OptionParser

case class Config(randomSeed: Option[Int] = None,
                  specFileName: Option[String] = None,
                  blockInterval: Int = Config.DefaultBlockInterval,
                  duration: Int = Config.DefaultDuration,
                  feePerWeight: Value = Config.DefaultFeePerWeight,
                  numDelayAttackNodes: Int = 0,
                  numLoopAttackNodes: Int = 0,
                  disableCriticalChannels: Int = 0)

object Config {
  val DefaultBlockInterval: Int = 10 * 60
  val DefaultDuration: Int = 7 * 24 * 60 * 60
  val DefaultFeePerWeight: Value = 2500

  val parser = new OptionParser[Config]("lnsim") {
    head("lnsim", "0.0.1")

    help("help")

    opt[String]("spec-file")
      .required()
      .action((fileName, c) => c.copy(specFileName = Some(fileName)))
      .text("path to JSON specification file")

    opt[Int]("seed")
      .action((seed, c) => c.copy(randomSeed = Some(seed)))
      .text("random seed")

    opt[Int]("block-interval")
      .action((blockInterval, c) => c.copy(blockInterval = blockInterval))
      .text(s"expected time between blocks in seconds (default: $DefaultBlockInterval)")

    opt[Int]("duration")
      .action((blockInterval, c) => c.copy(blockInterval = blockInterval))
      .text(s"length of simulated run in seconds (default: $DefaultDuration)")

    opt[Int]("fee-per-weight")
      .action((feePerWeight, c) => c.copy(feePerWeight = feePerWeight))
      .text(s"static fee rate used in the simulation in mSAT (default: $DefaultFeePerWeight")

    opt[Int]("num-delay-attack-nodes")
      .action((numDelayAttackNodes, c) => c.copy(numDelayAttackNodes = numDelayAttackNodes))
      .text("the number of delay attacking nodes (default: 0)")

    opt[Int]("num-loop-attack-nodes")
      .action((numLoopAttackNodes, c) => c.copy(numLoopAttackNodes = numLoopAttackNodes))
      .text("the number of loop attacking nodes (default: 0)")

    opt[Int]("disable-critical-channels")
      .action((disableCriticalChannels, c) =>
        c.copy(disableCriticalChannels = disableCriticalChannels))
      .text("enable an attacker that can disable this many channels of its choosing")
  }
}

