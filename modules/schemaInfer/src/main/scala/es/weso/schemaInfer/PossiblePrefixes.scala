package es.weso.schemaInfer
import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PREFIXES._

object PossiblePrefixes {

  val wikidataId = "Endpoint(https://query.wikidata.org/sparql)"
  val wikibase = IRI("http://wikiba.se/ontology#")
  val schemaIri = IRI("http://schema.org/")
  val wikidata = IRI("http://www.wikidata.org/")
  val propIri   = wikidata + "prop/"
  val wdt = wikidata + "prop/direct/"
  val propRef = wikidata + "prop/reference/"
  val propRefValue = wikidata + "prop/reference/value/"
  val skos = IRI("http://www.w3.org/2004/02/skos/core#")
  val wdRef = IRI("http://www.wikidata.org/reference/")
  val `wikibase:directClaim` = wikibase + "directClaim"

  val wikidataPrefixMap = PrefixMap(Map(
    Prefix("cc") -> IRI("http://creativecommons.org/ns#"),
    Prefix("dct") -> IRI("http://purl.org/dc/terms/"),
    Prefix("owl") -> IRI("http://www.w3.org/2002/07/owl#"),
    Prefix("pq") -> IRI("http://www.wikidata.org/prop/qualifier/"),
    Prefix("pqv") -> IRI("http://www.wikidata.org/prop/qualifier/value/"),
    Prefix("pqvn") -> IRI("http://www.wikidata.org/prop/qualifier/value-normalized/"),
    Prefix("pr") -> propRef,
    Prefix("prn") -> IRI("http://www.wikidata.org/prop/reference/value-normalized/"),
    Prefix("prv") -> propRefValue,
    Prefix("prov") -> IRI("http://www.w3.org/ns/prov#"),
    Prefix("ps") -> IRI("http://www.wikidata.org/prop/statement/"),
    Prefix("psv") -> IRI("http://www.wikidata.org/prop/statement/value/"),
    Prefix("psvn") -> IRI("http://www.wikidata.org/prop/statement/value-normalized/"),
    Prefix("rdf") -> rdf,
    Prefix("rdfs") -> rdfs,
    Prefix("schema") -> schemaIri,
    Prefix("skos") -> skos,
    Prefix("wdno") -> IRI("http://www.wikidata.org/prop/novalue/"),
    Prefix("wdt") -> wdt,
    Prefix("wdtn") -> IRI("http://www.wikidata.org/prop/direct-normalized/"),
    Prefix("wdp") -> propIri,
    Prefix("wdref") -> wdRef,
    Prefix("wikibase") -> wikibase,
    Prefix("wdata") -> IRI("http://www.wikidata.org/wiki/Special:EntityData/"),
    Prefix("xsd") -> xsd
  ))

}