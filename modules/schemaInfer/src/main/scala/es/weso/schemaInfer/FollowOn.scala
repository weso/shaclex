package es.weso.schemaInfer
import es.weso.rdf.nodes.IRI
import SchemaInfer.{propIri}

case class FollowOn(check: (IRI,IRI) => Either[String,IRI])
//,
//                    pred: IRI
//                   )

object FollowOn {

  val prov = IRI("http://www.w3.org/ns/prov#")
  val `prov:wasDerivedFrom` = prov + "wasDerivedFrom"
  val wdPropRegex = s"^${propIri.str}(P\\d*)".r

  def checkWDProp(label: IRI, prop: IRI): Either[String, IRI] = {
    prop.str match {
      case wdPropRegex(prop) => Right(label.resolve(IRI(prop + "Prop")))
      case _ => Left(s"$prop does not match $wdPropRegex")
    }
  }

  def checkWasDerivedFrom(label:IRI, prop: IRI): Either[String, IRI] = {
    prop match {
      case `prov:wasDerivedFrom` => Right(label + "Ref")
      case _ => Left(s"$prop does not match ${`prov:wasDerivedFrom`}")
    }
  }

  val followOnReference = FollowOn(checkWDProp)
  val followOnWasDerivedFrom = FollowOn(checkWasDerivedFrom)

}