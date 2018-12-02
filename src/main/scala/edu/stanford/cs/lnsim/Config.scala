package edu.stanford.cs.lnsim

import scopt.OptionParser

case class Config(randomSeed: Option[Int] = None,
                  specFileName: Option[String] = None,
                  blockInterval: Int = Config.DefaultBlockInterval,
                  duration: Int = Config.DefaultDuration,
                  feePerWeight: Value = Config.DefaultFeePerWeight)

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
  }
}

