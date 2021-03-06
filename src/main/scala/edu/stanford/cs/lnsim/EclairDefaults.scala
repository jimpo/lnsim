package edu.stanford.cs.lnsim

object EclairDefaults {
  val ExpiryDelta: BlockDelta = 144
  val FeeBase: Value = 1000
  val FeeProportionalMillionths: Long = 100
  val DustLimitSatoshis: Value = 546000
  val MinFundingAmount: Value = 100000000L

  val HTLCMinimum: Value = 1
  val MaxAcceptedHTLCs: Int = 30
  val MaxHTLCInFlight: Value = 5000000000L

  val ToRemoteDelay: BlockDelta = 144
  val MaxToLocalDelay: BlockDelta = 2000

  val ReserveToFundingRatio: Double = 0.01
  val MaxReserveToFundingRatio: Double = 0.05

  val FinalExpiryDelta: BlockDelta = 9
  val MinDepthBlocks: BlockDelta = 3

  // Channel.MIN_CLTV_EXPIRY, Channel.MAX_CLTV_EXPIRY
  val MinExpiry: BlockDelta = 9
  val MaxExpiry: BlockDelta = 7 * 144
}
