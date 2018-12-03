package edu.stanford.cs.lnsim.spec

import edu.stanford.cs.lnsim.des.Timestamp

case class SimulationSpec(nodes: List[NodeSpec],
                          transactions: List[TransactionSpec],
                          channelBudgets: List[ChannelBudgetSpec],
                          startTime: Timestamp,
                          endTime: Timestamp)
