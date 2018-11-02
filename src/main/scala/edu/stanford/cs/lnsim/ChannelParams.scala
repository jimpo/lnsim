package edu.stanford.cs.lnsim

case class ChannelParams(requiredReserve: Value,
                         dustLimit: Value,
                         maxHTLCInFlight: Value,
                         maxAcceptedHTLCs: Value,
                         htlcMinimum: Value)
