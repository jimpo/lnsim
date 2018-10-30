package edu.stanford.cs.lnsim

import spray.json.{JsObject, JsString, JsValue, RootJsonFormat}
import spray.json.DefaultJsonProtocol._

object LNSJSONProtocol {
  val ChannelUpdateFormat : RootJsonFormat[ChannelUpdate] = jsonFormat7(ChannelUpdate)

  implicit object ChannelFormat extends RootJsonFormat[Channel] {
    def write(c: Channel) = JsObject(
      "id" -> JsString(c.id.toString),
      "node1" -> JsString(c.node1.toString),
      "node2" -> JsString(c.node2.toString))
    def read(value: JsValue): Channel = ???
  }

  implicit object NodeFormat extends RootJsonFormat[Node] {
    def write(n: Node) = JsObject("id" -> JsString(n.id.toString))
    def read(value: JsValue): Node = ???
  }
}
