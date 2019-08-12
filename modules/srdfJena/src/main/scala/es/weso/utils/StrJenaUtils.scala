package es.weso.utils
import org.apache.jena.atlas.lib._

object StrJenaUtils {
  def unescape(str: String): String = {
    EscapeStr.unescape(str, '\\', false)
  }
}