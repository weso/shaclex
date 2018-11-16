package es.weso.schemaInfer
import es.weso.rdf.nodes.IRI

case class FollowOn(check: IRI => Boolean,
                    pred: IRI,
                    labelGenerator: IRI => IRI)

object FollowOn {

  val prov = IRI("http://www.w3.org/ns/prov#")
  val `prov:wasDerivedFrom` = prov + "wasDerivedFrom"

  val checkWDProp: IRI => Boolean = iri  => {
    println(s"Checking wd property: $iri")
    false
  }
  val wdPropGen: IRI => IRI = identity

  val followOnReference = FollowOn(checkWDProp, `prov:wasDerivedFrom`, wdPropGen)

}