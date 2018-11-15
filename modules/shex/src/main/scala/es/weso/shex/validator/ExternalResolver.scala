package es.weso.shex.validator
import es.weso.rdf.nodes.IRI
import es.weso.shex.{Annotation, Schema, ShapeExpr, ShapeLabel}

sealed abstract class ExternalResolver {
  def getShapeExpr(label: ShapeLabel,
                   as: Option[List[Annotation]]
                  ): Either[String, ShapeExpr]
}

// TODO: Should we have a list of IRIs instead of a single one?
case class ExternalIRIResolver(maybeIri: Option[IRI]) extends ExternalResolver {

  lazy val eitherSchema: Either[String,Schema] = maybeIri match {
    case None => Left(s"No IRI provided for ExternalIRI resolver")
    case Some(iri) => Schema.fromIRI(iri,maybeIri)
  }

  override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): Either[String,ShapeExpr] = for {
   schema <- eitherSchema
   se <- schema.getShape(label)
  } yield se

}

case object NoAction extends ExternalResolver {
  override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): Either[String,ShapeExpr] =
    Left(s"NoAction resolver can't obtain external shape expression from label: $label")
}