package edu.stanford.cs.lnsim.log

import org.apache.logging.log4j.{Level, Logger}
import spray.json.{JsField, JsObject, JsValue}

class StructuredLogger(val logger: Logger) {
  def trace(fields: JsField*): Unit = log(Level.TRACE, fields:_*)
  def debug(fields: JsField*): Unit = log(Level.DEBUG, fields:_*)
  def info(fields: JsField*): Unit = log(Level.INFO, fields:_*)
  def warn(fields: JsField*): Unit = log(Level.WARN, fields:_*)
  def error(fields: JsField*): Unit = log(Level.ERROR, fields:_*)
  def fatal(fields: JsField*): Unit = log(Level.FATAL, fields:_*)

  def log(l: Level, fields: JsField*): Unit =
    logger.log(l, JsObject(StructuredLogger.globals.toSeq ++ fields:_*))
}

object StructuredLogger {
  private var globals: Map[String, JsValue] = Map.empty

  def setGlobal(field: JsField): Unit = globals += field
  def unsetGlobal(key: String): Unit = globals -= key
}
