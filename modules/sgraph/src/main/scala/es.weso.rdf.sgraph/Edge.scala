package es.weso.rdf.sgraph

case class Edge(n1: Node, n2: Node, label: String, href: String) {
  def toDot(prefs: RDFDotPreferences): String = {
    s"""${n1.id} -> ${n2.id} [label = "$label", href = "$href"] ;"""
  }
}
