package es.weso.rdf.sgraph

import io.circe.Json

case class Edge(n1: Node, n2: Node, label: String, href: String) {

  def toDot(prefs: RDFDotPreferences): String = {
    s"""${n1.id} -> ${n2.id} [label = "$label", href = "$href"] ;"""
  }

  def toJson: Json = Json.fromFields(
    List(("data",Json.fromFields(
        List(
          ("source", Json.fromString(n1.id)),
          ("target", Json.fromString(n2.id)),
          ("label", Json.fromString(label)),
          ("href", Json.fromString(href))
        ))
    ))
  )
}
