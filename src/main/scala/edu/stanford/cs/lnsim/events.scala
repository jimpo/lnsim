package edu.stanford.cs.lnsim

package object events {
  sealed trait Base
  case object Start extends Base
  case class NewBlock(number: Int) extends Base
}

