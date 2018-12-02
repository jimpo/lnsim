package edu.stanford.cs.lnsim.spec

import edu.stanford.cs.lnsim.des.Timestamp
import edu.stanford.cs.lnsim.{NodeID, PaymentID, Value}

case class TransactionSpec(timestamp: Timestamp,
                           sender: NodeID,
                           recipient: NodeID,
                           amount: Value,
                           paymentID: PaymentID)
