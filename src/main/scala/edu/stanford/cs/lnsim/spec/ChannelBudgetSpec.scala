package edu.stanford.cs.lnsim.spec

import edu.stanford.cs.lnsim.{NodeID, Value}
import edu.stanford.cs.lnsim.des.Timestamp

case class ChannelBudgetSpec(node: NodeID, timestamp: Timestamp, amount: Value)
