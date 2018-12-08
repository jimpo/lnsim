package edu.stanford.cs.lnsim.graph

import edu.stanford.cs.lnsim.{ChannelID, NodeID}

import scala.collection.mutable.ArrayBuffer

/** Node represents a vertex in the adjacency graph of the network. Stores a reference to all
  * incidental edges.
  *
  * Implementation note: the edges are stored in a ArrayBuffer because iterating over them in the
  * route finding algorithm is more expensive than the infrequent updates.
  */
class Node(val id: NodeID) {
  private val channels: ArrayBuffer[Channel] = ArrayBuffer.empty

  def updateChannel(channel: Channel): Unit = {
    val index = channels.indexWhere(_.id == channel.id)
    if (index < 0) {
      channels.append(channel)
    } else if (channels(index).lastUpdate < channel.lastUpdate) {
      channels(index) = channel
    }
  }

  def removeChannel(channelID: ChannelID): Unit = {
    val index = channels.indexWhere(_.id == channelID)
    if (index >= 0) {
      // Constant time remove
      channels(index) = channels.last
      channels.remove(channels.length - 1)
    }
  }

  def channel(channelID: ChannelID): Option[Channel] = channels.find(_.id == channelID)
  def channelCount: Int = channels.length
  def channelsIterator: Iterator[Channel] = channels.iterator

  // Copied implementation from ResizeableArray, except without the asInstanceOf cast. This seems
  // to have a minor but noticeable effect on performance.
  def foreachChannel(fn: Channel => Unit): Unit = {
    var i = 0
    val top = channels.length
    while (i < top) {
      fn(channels(i))
      i += 1
    }
  }
}
