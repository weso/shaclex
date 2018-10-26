package es.weso.shex

import java.nio.file.{Files, Paths}

import cats._
import cats.implicits._
import es.weso.depgraphs.DepGraph
import es.weso.rdf.{PrefixMap, RDFBuilder, RDFReader}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}

import scala.io.Source
import scala.util.{Either, Left, Right, Try}

case class Schema(id: IRI,
                  prefixes: Option[PrefixMap],
                  base: Option[IRI],
                  startActs: Option[List[SemAct]],
                  start: Option[ShapeExpr],
                  shapes: Option[List[ShapeExpr]],
                  tripleExprMap: Option[Map[ShapeLabel,TripleExpr]],
                  imports: List[IRI]
                 ) {

  def addShape(se: ShapeExpr): Schema = this.copy(shapes = addToOptionList(se,shapes))

  private def addToOptionList[A](x: A, maybeLs: Option[List[A]]): Option[List[A]] = maybeLs match {
    case None => Some(List(x))
    case Some(xs) => Some(x :: xs)
  }

  def getTripleExpr(lbl: ShapeLabel): Option[TripleExpr] =
    tripleExprMap.map(_.get(lbl)).getOrElse(None)

  def resolveShapeLabel(l: ShapeLabel): Either[String, IRI] = l match {
    case IRILabel(iri) => Right(iri)
    case _ => Left(s"Label $l can't be converted to IRI")
  }

  lazy val prefixMap: PrefixMap =
    prefixes.getOrElse(PrefixMap.empty)

  def getTripleExprMap(): Map[ShapeLabel, TripleExpr] =
    tripleExprMap.getOrElse(Map())

  lazy val localShapesMap: Map[ShapeLabel,ShapeExpr] = {
    shapes match {
      case None => Map()
      case Some(ls) => {
        ls.collect{ case s if s.id.isDefined => (s.id.get, s)}.toMap
      }
    }
  }

  lazy val eitherResolvedShapesMap: Either[String,Map[ShapeLabel,ShapeExpr]] = {
    closureImports(imports, List(id), localShapesMap)
  }

  // TODO: make the following method tailrecursive
  private def closureImports(imports: List[IRI],
                             visited: List[IRI],
                             current: Map[ShapeLabel,ShapeExpr]
                            ): Either[String, Map[ShapeLabel,ShapeExpr]] = imports match {
    case Nil => Right(current)
    case (i::is) => if (visited contains i) closureImports(is,visited,current)
    else for {
      schema <- Schema.fromIRI(i)
      sm <- closureImports(is ++ schema.imports, i :: visited, schema.localShapesMap ++ current)
    } yield sm
  }

  def addId(i: IRI): Schema = this.copy(id = i)


  def qualify(node: RDFNode): String =
    prefixMap.qualify(node)

  def qualify(label: ShapeLabel): String =
    prefixMap.qualify(label.toRDFNode)

  def getShape(label: ShapeLabel): Either[String,ShapeExpr] = for {
    sm <- eitherResolvedShapesMap
    se <- sm.get(label) match {
      case None => Left(s"Not found $label in schema. Available labels: ${sm.keySet.mkString}")
      case Some(se) => Right(se)
    }
  } yield se

  lazy val localShapes: List[ShapeExpr] = shapes.getOrElse(List())

  lazy val shapeList: List[ShapeExpr] = // shapes.getOrElse(List())
   eitherResolvedShapesMap.fold(_ => localShapes,
     sm => sm.values.toList)

  def labels: List[ShapeLabel] = {
    eitherResolvedShapesMap.fold(
      e => localShapes.map(_.id).flatten,
      sm => sm.keySet.toList)
  }

  def addTripleExprMap(te: Map[ShapeLabel,TripleExpr]): Schema =
    this.copy(tripleExprMap = Some(te))

  def negCycles: Either[String, Set[Set[ShapeLabel]]] =
    Dependencies.negCycles(this)

  def depGraph: Either[String, DepGraph[ShapeLabel]] =
    Dependencies.depGraph(this)

}


object Schema {

  def rdfDataFormats(rdfReader: RDFReader) = rdfReader.availableParseFormats.map(_.toUpperCase) //   RDFAsJenaModel.availableFormats.map(_.toUpperCase)

  def empty: Schema =
    Schema(IRI(""),None, None, None, None, None, None, List())

  def fromIRI(i: IRI): Either[String, Schema] = {
    Try {
      val uri = i.uri
      if (uri.getScheme == "file") {
        if (Files.exists(Paths.get(i.uri))) {
            val str = Source.fromURI(uri).mkString
            fromString(str, "ShExC", Some(i.str)).map(schema => schema.addId(i))
        } else {
          val iriShEx = i + ".shex"
          if (Files.exists(Paths.get((iriShEx).uri))) {
            val str = Source.fromURI(iriShEx.uri).mkString
            fromString(str, "ShExC", Some(i.str)).map(schema => schema.addId(i))
          } else {
            val iriJson = i + ".json"
            if (Files.exists(Paths.get((iriJson).uri))) {
            val str = Source.fromURI(iriJson.uri).mkString
            fromString(str, "JSON", Some(i.str)).map(schema => schema.addId(i))
           }
             else Left(s"File $i does not exist")
        }}}
      else {
        val str = Source.fromURI(i.uri).mkString
        fromString(str, "ShExC", Some(i.str)).map(schema => schema.addId(i))
      }
    }.fold(exc => Left(exc.getMessage), identity)

  }

  /**
  * Reads a Schema from a char sequence
    * @param cs char sequence
    * @param format syntax format
    * @param base base URL
    * @param rdfReader RDFReader value from which to obtain RDF data formats (in case of RDF format)
    * @return either a Schema or a String message error
    */
  def fromString(cs: CharSequence,
                 format: String = "ShExC",
                 base: Option[String] = None,
                 maybeRDFReader: Option[RDFReader] = None
                ): Either[String, Schema] = {
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
      case _ => maybeRDFReader match {
        case None => Left(s"Not implemented ShEx parser for format $format and no rdfReader provided")
        case Some(rdfReader) =>
         if (rdfDataFormats(rdfReader).contains(formatUpperCase)) for {
          rdf    <- rdfReader.fromString(cs, formatUpperCase, base)
          schema <- RDF2ShEx.rdf2Schema(rdf)
         } yield schema
         else Left(s"Not implemented ShEx parser for format $format")
       }
    }
  }
  def serialize(schema: Schema,
                format: String,
                rdfBuilder: RDFBuilder): Either[String,String] = {
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
      case _ if (rdfDataFormats(rdfBuilder).contains(formatUpperCase)) => {
        val rdf = ShEx2RDF(schema, None, rdfBuilder.empty)
        rdf.serialize(formatUpperCase)
      }
      case _ =>
        Left(s"Not implemented conversion to $format. Schema: $schema")
    }
  }
}
