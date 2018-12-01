package edu.stanford.cs.lnsim.node

import edu.stanford.cs.lnsim._

class BlockchainView(private val nodeID: NodeID, private val blockchain: Blockchain) {
  def blockNumber: BlockNumber = blockchain.blockNumber

  def newFundingTransaction(channelID: ChannelID): Unit =
    blockchain.newFundingTransaction(channelID)

  def subscribeChannelConfirmed(channelID: ChannelID, confirmations: BlockDelta): Boolean =
    blockchain.subscribeChannelConfirmed(nodeID, channelID, confirmations)

  def subscribeAction(subscribeNumber: BlockNumber, action: NodeAction): Boolean =
    blockchain.subscribeAction(subscribeNumber, nodeID, action)
}
