package edu.stanford.cs.lnsim

class BlockchainView(private val nodeID: NodeID, private val blockchain: Blockchain) {
  def blockNumber: BlockNumber = blockchain.blockNumber

  def newFundingTransaction(channelID: ChannelID): Unit =
    blockchain.newFundingTransaction(channelID)

  def subscribeChannelConfirmed(channelID: ChannelID, confirmations: BlockDelta): Boolean =
    blockchain.subscribeChannelConfirmed(nodeID, channelID, confirmations)

}
