package edu.stanford.cs.lnsim

sealed trait OnChainEvent
case class ChannelOpened(channelID: ChannelID) extends OnChainEvent
