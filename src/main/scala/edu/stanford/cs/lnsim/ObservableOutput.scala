package edu.stanford.cs.lnsim

trait ObservableOutput {
  def openChannel(channelID: ChannelID,
                  initiatingNode: NodeID,
                  receivingNode: NodeID,
                  capacity: Value,
                  fee: Value): Unit
}
