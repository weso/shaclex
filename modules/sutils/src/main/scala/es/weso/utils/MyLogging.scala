package es.weso.utils

/* Naïve logging only for debugging */
trait MyLogging {
  class Logger {
    def debug(str:String): Unit = {
      println(s"DEBUG: $str")
    }
    def info(str:String): Unit = {
      println(s"DEBUG: $str")
    }
  }
  val logger = new Logger
}
