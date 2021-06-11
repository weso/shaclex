package es.weso.schemaInfer
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes.IRI
import es.weso.shex.{Schema, ShapeExpr}
import cats.data._
import cats.implicits._
import cats.effect.IO

case class InferredSchema(smap: Map[IRI, InferredShape]) extends AnyVal {

  def get(label: IRI): Option[InferredShape] =
    smap.get(label)

  def updated(label: IRI, shape: InferredShape): InferredSchema =
    InferredSchema(smap.updated(label,shape))

  def values: List[InferredShape] = smap.values.toList

  type ES[A] = Either[String,A]

  def toShExSchema(rdf: RDFReader,
                   opts: InferOptions,
                   pm: PrefixMap
                  ): EitherT[IO,String, Schema] = { 
  val rs: List[EitherT[IO,String,ShapeExpr]] = 
    smap.toList.map { case (iri, is) => is.toShapeExpr(Some(iri),opts, rdf) }                    
  for {
    es <- rs.sequence
  } yield Schema(IRI(""), Some(pm), None, None, None, Some(es),None,List(), None)
 }
}

object InferredSchema {
  def empty: InferredSchema = InferredSchema(Map())
}