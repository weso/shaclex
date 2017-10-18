package es.weso.rdf

import es.weso.rdf.nodes.IRI

/**
 * Common Prefixes for RDF
 */
object PREFIXES {

  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val owl = IRI("http://www.w3.org/2002/07/owl#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val sh = IRI("http://www.w3.org/ns/shacl#")

  lazy val xsd_string = xsd.add("string")
  lazy val xsd_byte = xsd.add("byte")
  lazy val xsd_decimal = xsd.add("decimal")
  lazy val xsd_double = xsd.add("double")
  lazy val xsd_int = xsd.add("int")
  lazy val xsd_integer = xsd.add("integer")
  lazy val xsd_long = xsd.add("long")
  lazy val xsd_positiveInteger = xsd.add("positiveInteger")
  lazy val xsd_negativeInteger = xsd.add("negativeInteger")
  lazy val xsd_nonNegativeInteger = xsd.add("nonNegativeInteger")
  lazy val xsd_nonPositiveInteger = xsd.add("nonPositiveInteger")
  lazy val xsd_short = xsd.add("short")
  lazy val xsd_unsignedLong = xsd.add("unsignedLong")
  lazy val xsd_unsignedInt = xsd.add("unsignedInt")
  lazy val xsd_unsignedShort = xsd.add("unsignedShort")
  lazy val xsd_unsignedByte = xsd.add("unsignedByte")
  lazy val xsd_float = xsd.add("float")
  lazy val xsd_anyUri = xsd.add("anyUri")
  lazy val xsd_boolean = xsd.add("boolean")

  lazy val rdf_type = rdf.add("type")
  lazy val rdf_nil = rdf.add("nil")
  lazy val rdf_first = rdf.add("first")
  lazy val rdf_rest = rdf.add("rest")
  lazy val rdf_langString = rdf.add("langString")

  private val basicMap: Map[String, IRI] =
    Map(
      "rdf" -> rdf,
      "xsd" -> xsd,
      "rdfs" -> rdfs,
      "owl" -> owl,
      "sh" -> sh)

}
