package edu.stanford.cs.lnsim

import scopt.OptionParser

case class Config(
  randomSeed: Option[Int] = None,
  blockInterval: Int = Config.DefaultBlockInterval,
  duration: Int = Config.DefaultDuration,
                 )

object Config {
  val DefaultBlockInterval: Int = 10 * 60
  val DefaultDuration: Int = 7 * 24 * 60 * 60

  val parser = new OptionParser[Config]("lnsim") {
    head("lnsim", "0.0.1")

    help("help")

    opt[Int]("seed")
      .action((seed, c) => c.copy(randomSeed = Some(seed)))
      .text("random seed")

    opt[Int]("block-interval")
      .action((blockInterval, c) => c.copy(blockInterval = blockInterval))
      .text(s"expected time between blocks in seconds (default: $DefaultBlockInterval)")

    opt[Int]("duration")
      .action((blockInterval, c) => c.copy(blockInterval = blockInterval))
      .text(s"length of simulated run in seconds (default: $DefaultDuration)")
  }
}

