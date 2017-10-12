package es.weso.schema
import cats._
import com.typesafe.scalalogging.LazyLogging
import data._
import implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapeMaps._
import es.weso.shex.{ Schema => Schema_, _ }
import es.weso.shex.validator._
import es.weso.shex._
import es.weso.typing._
import es.weso.shex.shexR._

import scala.util._
import es.weso.shex.implicits.showShEx.showShapeLabel

case class ShExSchema(schema: Schema_) extends Schema with LazyLogging {
  override def name = "ShEx"

  lazy val shExCFormat = "ShExC"
  lazy val shExJFormat = "ShExJ"
  lazy val validator = Validator(schema)

  override def formats =
    List(shExCFormat, shExJFormat) ++
      RDFAsJenaModel.availableFormats

  lazy val formatsUpperCase = formats.map(_.toUpperCase)

  override def defaultTriggerMode: ValidationTrigger = ShapeMapTrigger.empty

  override def validate(rdf: RDFReader, trigger: ValidationTrigger): Result = (trigger match {
    case TargetDeclarations => validateTargetDecls(rdf)
    //    case MapTrigger(sm, ns) => validateShapeMap(sm, ns, rdf)
    case ShapeMapTrigger(sm) => {
      ShapeMap.fixShapeMap(sm, rdf, rdf.getPrefixMap(), schema.prefixes.getOrElse(PrefixMap.empty)) match {
        case Left(msg) => Result.errStr(s"Error fixing shape map: $msg")
        case Right(fixedShapeMap) => validateFixedShapeMap(fixedShapeMap, rdf)
      }
    }
  }).addTrigger(trigger)

  def validateTargetDecls(rdf: RDFReader): Result = {
    val r = validator.validateNodeDecls(rdf)
    cnvResult(r, rdf)
  }

  def cnvResult(r: Either[String, ResultShapeMap], rdf: RDFReader): Result = r match {
    case Left(msg) => Result(
      false,
      message = "Error validating",
      shapeMaps = Seq(),
      errors = Seq(ErrorInfo(msg)), None, rdf.getPrefixMap(), schema.prefixMap)
    case Right(resultShapeMap) =>
      Result(true, "Validated",
        shapeMaps = Seq(resultShapeMap),
        errors = Seq(), None, rdf.getPrefixMap(), schema.prefixMap)
  }

  def validateNodeShape(node: IRI, shape: String, rdf: RDFReader): Result = {
    val validator = Validator(schema)
    val r = validator.validateNodeShape(rdf, node, shape)
    cnvResult(r, rdf)
  }

  def validateNodeStart(node: IRI, rdf: RDFReader): Result = {
    val validator = Validator(schema)
    val r = validator.validateNodeStart(rdf, node)
    cnvResult(r, rdf)
  }

  def validateFixedShapeMap(fixedShapeMap: FixedShapeMap, rdf: RDFReader): Result = {
    validateShapeMap(fixedShapeMap, rdf)
  }

  def validateShapeMap(
    shapeMap: FixedShapeMap,
    rdf: RDFReader): Result = {
    Validator.validate(schema, shapeMap, rdf) match {
      case Left(error) =>
        Result(false, "Error validating", Seq(), Seq(ErrorInfo(error)), None, rdf.getPrefixMap(), schema.prefixMap)
      case Right(resultShapeMap) =>
        Result(true, "Validated", Seq(resultShapeMap), Seq(), None, rdf.getPrefixMap(), schema.prefixMap)
    }
  }

  /*  def cnvResult(r: CheckResult[ViolationError, ShapeTyping, List[(es.weso.shex.validator.NodeShape, String)]], rdf: RDFReader): Result =
    Result(
      isValid = r.isOK,
      message = if (r.isOK) "Valid" else "Not valid",
      shapeMaps = r.results.map(cnvShapeTyping(_, rdf)),
      errors = r.errors.map(cnvViolationError(_)),
      None,
      rdf.getPrefixMap(),
      schema.prefixMap)

  def cnvShapeTyping(st: ShapeTyping, rdf: RDFReader): ResultShapeMap = ???  */
  /*{
    Solution(
      st.t.getMap.mapValues(cnvResult),
      rdf.getPrefixMap(),
      schema.prefixMap)
  }

  def cnvResult(
    r: Map[ShapeType, TypingResult[ViolationError, String]]): InfoNode = {
    val (oks, bads) = r.toSeq.partition(_._2.isOK)
    InfoNode(
      oks.map(cnvShapeResult(_)),
      bads.map(cnvShapeResult(_)),
      schema.prefixMap)
  } */

  /*  def cnvShapeResult(
    p: (ShapeType, TypingResult[ViolationError, String])): (SchemaLabel, Explanation) = {
    val shapeLabel = p._1.label match {
      case Some(lbl) => {
        val str = lbl.toRDFNode.getLexicalForm
        println(s"Converting shapeLabel: $str")
        SchemaLabel(str, schema.prefixMap)
      }
      case None => SchemaLabel("_", schema.prefixMap)
    }
    val explanation = Explanation(cnvTypingResult(p._2))
    (shapeLabel, explanation)
  }

  def cnvTypingResult(result: TypingResult[ViolationError, String]): String = {
    result.t.fold(
      es => "Errors: " +
        es.toList.mkString(","), rs => "Evidences:" +
        rs.map(" " + _).mkString(","))
  } */

  def cnvViolationError(v: ViolationError): ErrorInfo = {
    ErrorInfo(v.show)
  }

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Either[String, ShExSchema] = {
    ShExSchema.fromString(cs, format, base)
  }

  override def fromRDF(rdf: RDFReader): Either[String, Schema] =
    RDF2ShEx.rdf2Schema(rdf).map(ShExSchema(_))

  override def serialize(format: String): Either[String, String] = {
    if (formatsUpperCase.contains(format.toUpperCase()))
      Right(Schema_.serialize(schema, format))
    else
      Left(s"Can't serialize to format $format. Supported formats=$formats")
  }

  override def empty: ShExSchema = ShExSchema.empty

  override def shapes: List[String] = {
    val pm = schema.prefixMap
    schema.labels.map(lbl => pm.qualify(lbl.toRDFNode))
  }

  override def pm: PrefixMap = schema.prefixMap

}

object ShExSchema {
  def empty: ShExSchema = ShExSchema(schema = Schema_.empty)

  def fromString(
    cs: CharSequence,
    format: String,
    base: Option[String]): Either[String, ShExSchema] = {
    Schema_.fromString(cs, format, base).map(p => ShExSchema(p))
  }

}
