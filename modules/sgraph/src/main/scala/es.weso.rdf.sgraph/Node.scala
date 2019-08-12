package es.weso.rdf.sgraph

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import io.circe.Json

case class Node(id: String, label: String, rdfNode: RDFNode, pm: PrefixMap) {

  private def jsonLabel(node: RDFNode): Json = node match {
    case _: IRI => Json.fromString(label)
    case _: BNode => Json.fromString("")
    case l: Literal => Json.fromString(labelLiteral(l))
  }

  private def jsonType(node: RDFNode): Json = node match {
    case _: IRI => Json.fromString("iri")
    case _: BNode => Json.fromString("bNode")
    case _: Literal => Json.fromString("lit")
  }

  private def labelLiteral(l: Literal): String = l match {
    case s: StringLiteral => s.getLexicalForm
    case i: IntegerLiteral => i.int.toString
    case l: LangLiteral => l.lexicalForm + "@" + l.lang.lang
    case dt: DatatypeLiteral => l.getLexicalForm + "^^" + pm.qualify(l.dataType)
    case _ => l.getLexicalForm
  }

  def toDot(dotPreferences: RDFDotPreferences): String = rdfNode match {
    case i: IRI => s"""node [shape=${dotPreferences.irisPrefs.shape.name}, style=${dotPreferences.irisPrefs.style.name}, color=${dotPreferences.irisPrefs.color.name}, label="$label", href="${i.str}"] $id ;"""
    case l: Literal => s"""node[shape=${dotPreferences.literalPrefs.shape.name}, style=${dotPreferences.literalPrefs.style.name}, color=${dotPreferences.literalPrefs.color.name}, label="${labelLiteral(l)}"] $id ;"""
    case _: BNode => s"""node[shape=${dotPreferences.bnodesPrefs.shape.name}, style=${dotPreferences.bnodesPrefs.style.name}, color=${dotPreferences.bnodesPrefs.color.name}, label=""] $id ;"""
  }

  def toJson: Json = Json.fromFields(List(("data",
      Json.fromFields(List(
        ("id", Json.fromString(id)),
        ("label", jsonLabel(rdfNode)),
        ("type", jsonType(rdfNode))
      ))
    )))

}
