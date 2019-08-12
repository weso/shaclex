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

  lazy val `xsd:string`: IRI = xsd.add("string")
  lazy val `xsd:byte`: IRI = xsd.add("byte")
  lazy val `xsd:dateTime`: IRI = xsd.add("dateTime")
  lazy val `xsd:decimal`: IRI = xsd.add("decimal")
  lazy val `xsd:double`: IRI = xsd.add("double")
  lazy val `xsd:int`: IRI = xsd.add("int")
  lazy val `xsd:integer`: IRI = xsd.add("integer")
  lazy val `xsd:long`: IRI = xsd.add("long")
  lazy val `xsd:positiveInteger`: IRI = xsd.add("positiveInteger")
  lazy val `xsd:negativeInteger`: IRI = xsd.add("negativeInteger")
  lazy val `xsd:nonNegativeInteger`: IRI = xsd.add("nonNegativeInteger")
  lazy val `xsd:nonPositiveInteger`: IRI = xsd.add("nonPositiveInteger")
  lazy val `xsd:short`: IRI = xsd.add("short")
  lazy val `xsd:unsignedLong`: IRI = xsd.add("unsignedLong")
  lazy val `xsd:unsignedInt`: IRI = xsd.add("unsignedInt")
  lazy val `xsd:unsignedShort`: IRI = xsd.add("unsignedShort")
  lazy val `xsd:unsignedByte`: IRI = xsd.add("unsignedByte")
  lazy val `xsd:float`: IRI = xsd.add("float")
  lazy val `xsd:anyUri`: IRI = xsd.add("anyUri")
  lazy val `xsd:boolean`: IRI = xsd.add("boolean")

  lazy val `rdf:type`: IRI = rdf.add("type")
  lazy val `rdf:nil`: IRI = rdf.add("nil")
  lazy val `rdf:first`: IRI = rdf.add("first")
  lazy val `rdf:rest`: IRI = rdf.add("rest")
  lazy val `rdf:langString`: IRI = rdf.add("langString")

  lazy val `rdfs:label`: IRI = rdfs.add("label")
  lazy val `rdfs:subClassOf`: IRI = rdfs.add("subClassOf")

  lazy val `sh:alternativePath`: IRI = sh + "alternativePath"
  lazy val `sh:inversePath`: IRI = sh + "inversePath"
  lazy val `sh:oneOrMorePath`: IRI = sh + "oneOrMorePath"
  lazy val `sh:zeroOrMorePath`: IRI = sh + "zeroOrMorePath"
  lazy val `sh:zeroOrOnePath`: IRI = sh + "zeroOrOnePath"

  val basicMap: Map[String, IRI] =
    Map(
      "rdf" -> rdf,
      "xsd" -> xsd,
      "rdfs" -> rdfs,
      "owl" -> owl,
      "sh" -> sh
    )

}
