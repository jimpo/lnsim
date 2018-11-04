package edu.stanford.cs.lnsim.log

import org.apache.logging.log4j.LogManager

trait StructuredLogging {
  protected val logger: StructuredLogger = new StructuredLogger(LogManager.getLogger(getClass))
}
