package es.weso.schema
import es.weso.rdf.PrefixMap

case class ErrorInfo(str: String) {
  def show(pm: PrefixMap): String = str
}
