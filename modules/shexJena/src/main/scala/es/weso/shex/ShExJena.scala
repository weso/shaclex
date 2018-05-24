package es.weso.shex

import cats.implicits._
import es.weso.depgraphs.DepGraph
import es.weso.rdf.{PrefixMap, RDFBuilder}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}

import scala.util.{Either, Left, Right}

case class Schema(prefixes: Option[PrefixMap],
                  base: Option[IRI],
                  startActs: Option[List[SemAct]],
                  start: Option[ShapeExpr],
                  shapes: Option[List[ShapeExpr]],
                  tripleExprMap: Option[Map[ShapeLabel,TripleExpr]],
                  imports: List[IRI]
                 ) {

  def resolveShapeLabel(l: ShapeLabel): Either[String, IRI] = l match {
    case IRILabel(iri) => Right(iri)
    case _ => Left(s"Label $l can't be converted to IRI")
  }

  lazy val prefixMap: PrefixMap =
    prefixes.getOrElse(PrefixMap.empty)

  def qualify(node: RDFNode): String =
    prefixMap.qualify(node)

  def qualify(label: ShapeLabel): String =
    prefixMap.qualify(label.toRDFNode)

  // TODO: Convert to Either[String,ShapeExpr]
  def getShape(label: ShapeLabel): Option[ShapeExpr] =
    shapes.getOrElse(List()).find(_.id == Some(label))

  lazy val shapeList = shapes.getOrElse(List())

  def labels: List[ShapeLabel] = {
    shapeList.map(_.id).flatten
  }

  def negCycles: Either[String, Set[Set[ShapeLabel]]] =
    Dependencies.negCycles(this)

  def depGraph: Either[String, DepGraph[ShapeLabel]] =
    Dependencies.depGraph(this)

}


object Schema {

  lazy val rdfDataFormats = RDFAsJenaModel.availableFormats.map(_.toUpperCase)

  def empty: Schema =
    Schema(None, None, None, None, None, None, List())

  def fromString(
                  cs: CharSequence,
                  format: String,
                  base: Option[String] = None): Either[String, Schema] = {
    val formatUpperCase = format.toUpperCase
    formatUpperCase match {
      case "SHEXC" => {
        import compact.Parser.parseSchema
        parseSchema(cs.toString, base)
      }
      case "SHEXJ" => {
        import io.circe.parser._
        import es.weso.shex.implicits.decoderShEx._
        decode[Schema](cs.toString).leftMap(_.getMessage)
      }
      case _ if (rdfDataFormats.contains(formatUpperCase)) => for {
        rdf <- RDFAsJenaModel.fromChars(cs, formatUpperCase, base)
        schema <- RDF2ShEx.rdf2Schema(rdf)
      } yield schema

      case _ => Left(s"Not implemented ShEx parser for format $format")
    }
  }

  def serialize(schema: Schema, format: String, rdfBuilder: RDFBuilder): Either[String,String] = {
    val formatUpperCase = format.toUpperCase
    formatUpperCase match {
      case "SHEXC" => {
        import compact.CompactShow._
        Right(showSchema(schema))
      }
      case "SHEXJ" => {
        import io.circe.syntax._
        import es.weso.shex.implicits.encoderShEx._
        Right(schema.asJson.spaces2)
      }
      case _ if (rdfDataFormats.contains(formatUpperCase)) => {
        val rdf = ShEx2RDF(schema, None, rdfBuilder.empty)
        rdf.serialize(formatUpperCase)
      }
      case _ =>
        Left(s"Not implemented conversion to $format. Schema: $schema")
    }
  }
}
