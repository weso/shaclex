package es.weso.shextest.manifest

import es.weso.rdf.nodes._

case class ShExManifest(
  label: Option[String],
  comment: Option[String],
  entries: List[Entry],
  includes: List[(RDFNode, Option[ShExManifest])])

object ShExManifest {
  def empty: ShExManifest = 
    ShExManifest(
      None, 
      None, 
      List(), 
      List()
    )
}



