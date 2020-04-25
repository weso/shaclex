package es.weso.schemaInfer
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.Lang
import es.weso.schemaInfer.SchemaInfer.SortFunction
import FollowOn._
import es.weso.rdf.nodes._

case class InferOptions(
                         inferTypePlainNode: Boolean,
                         addLabelLang: Option[Lang],
                         possiblePrefixMap: PrefixMap,
                         maxFollowOn: Int,
                         followOnLs: List[FollowOn],
                         followOnThreshold: Option[Int],
                         sortFunction: SortFunction,
                       )

object InferOptions {

  def orderByIRI: SortFunction = pm => (pair1,pair2) => {
    val (iri1,_) = pair1
    val (iri2,_) = pair2
    pm.qualify(iri1) < pm.qualify(iri2)
  }

  val defaultOptions =
    InferOptions(
      inferTypePlainNode = true,
      addLabelLang = Some(Lang("en")),
      possiblePrefixMap = PossiblePrefixes.wikidataPrefixMap,
      maxFollowOn = 1,
      followOnLs = List(
        followOnReference,
        followOnWasDerivedFrom,
        followOnStem(IRI("http://schema.org/")),
        followOnStem(IRI("http://www.w3.org/ns/td#"))
      ),
      followOnThreshold = Some(3),
      sortFunction = orderByIRI
    )
}
