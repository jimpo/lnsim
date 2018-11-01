package edu.stanford.cs.lnsim

package object des {
  /***
    * Timestamps are given in virtual milliseconds since the beginning of the simulation.
    */
  type Timestamp = Long

  /***
    * TimeDelta values are in virtual milliseconds.
    */
  type TimeDelta = Long

  def secondsToTimeDelta(seconds: Int): TimeDelta = seconds * 1000
}
