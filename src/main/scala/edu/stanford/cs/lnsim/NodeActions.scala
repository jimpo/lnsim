package edu.stanford.cs.lnsim

import edu.stanford.cs.lnsim.des.TimeDelta

trait NodeActions {
  def sendMessage(delay: TimeDelta, recipient: NodeID, message: Message): Unit
}
