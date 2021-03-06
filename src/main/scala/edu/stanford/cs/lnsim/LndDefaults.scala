package edu.stanford.cs.lnsim

object LndDefaults {
 val AutoPilotMinChannelSize: Value = 20000000L
 val AutoPilotMaxChannelSize: Value = 16777215000L
 val AutoPilotNumChannels: Int = 5

  // maxCltvExpiry in htlcswitch/link.go
  val MaxExpiry: BlockDelta = 5000
}
