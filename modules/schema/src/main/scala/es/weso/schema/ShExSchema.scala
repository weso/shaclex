package es.weso.schema

import cats._
import com.typesafe.scalalogging.LazyLogging
import implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.Shacl2ShEx
import es.weso.shapeMaps._
import es.weso.shex.{Schema => Schema_, _}
import es.weso.shex.validator._
import es.weso.shex._
import es.weso.shex.converter.ShEx2Shacl
import es.weso.shex.shexR._

import scala.util._

case class ShExSchema(schema: Schema_) extends Schema with LazyLogging {
  override def name = "ShEx"

  lazy val shExCFormat = "ShExC"
  lazy val shExJFormat = "ShExJ"
//  lazy val svgFormat = "SVG"
  lazy val validator = Validator(schema)

  // TODO: Separate input/output formats
  override def formats =
    List(shExCFormat, shExJFormat) ++
      RDFAsJenaModel.availableFormats
    // ++ List(svgFormat)

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
      validationReport = Left("No validation report in ShEx"),
      errors = Seq(ErrorInfo(msg)), None, rdf.getPrefixMap(), schema.prefixMap)
    case Right(resultShapeMap) =>
      Result(true, "Validated",
        shapeMaps = Seq(resultShapeMap),
        validationReport = Left(s"No validaton report in ShEx"),
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
        Result(false,
          "Error validating",
          Seq(),
          Left("No validation report yet"),
          Seq(ErrorInfo(error)),
          None,
          rdf.getPrefixMap(),
          schema.prefixMap)
      case Right(resultShapeMap) => {
        // println(s"Validated, result=$resultShapeMap")
        Result(true,
          "Validated",
          Seq(resultShapeMap),
          Left(s"No validation report for ShEx"),
          Seq(),
          None,
          rdf.getPrefixMap(),
          schema.prefixMap)
      }
    }
  }

  def cnvViolationError(v: ShExError): ErrorInfo = {
    ErrorInfo(v.show)
  }

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Either[String, ShExSchema] = {
    ShExSchema.fromString(cs, format, base)
  }

  override def fromRDF(rdf: RDFReader): Either[String, Schema] =
    RDF2ShEx.rdf2Schema(rdf).map(ShExSchema(_))

  override def serialize(format: String): Either[String, String] = {
    val fmt = format.toUpperCase
    val builder: RDFBuilder = RDFAsJenaModel.empty
    if (formatsUpperCase.contains(fmt))
      fmt match {
/*        case "SVG" => for {
          uml <- ShEx2UML.schema2Uml(schema)
        } yield uml.toSVG */
        case _ => Schema_.serialize(schema, fmt, builder)
      }
    else
      Left(s"Can't serialize to format $format. Supported formats=$formats")
  }

  override def empty: ShExSchema = ShExSchema.empty

  override def shapes: List[String] = {
    val pm = schema.prefixMap
    schema.labels.map(lbl => pm.qualify(lbl.toRDFNode))
  }

  override def pm: PrefixMap = schema.prefixMap

  def convert(targetFormat: Option[String], targetEngine: Option[String]): Either[String,String] = {
    targetEngine.map(_.toUpperCase) match {
      case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
      case Some(engine) if (engine.equalsIgnoreCase(name)) => {
        serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
      }
      case Some("SHACL") | Some("SHACLEX") =>
        for {
          newSchema <- ShEx2Shacl.shex2Shacl(schema).toEither.leftMap(es => es.toList.mkString("\n"))
          builder = RDFAsJenaModel.empty
          str <- newSchema.serialize(targetFormat.getOrElse(DataFormats.defaultFormatName),builder)
        } yield str
      case Some(unknown) => Left(s"Conversion from ShEx to $unknown not implemented yet")
    }
  }

  override def info: SchemaInfo = {

    val reasons: List[String] = schema.negCycles.fold(e => List(e), ns => {
      if (ns.isEmpty) List()
      else
        List(s"Negative cycles found: [${ns.map(s => s.map(_.toString).mkString(",")).mkString(",")}]")
    })
    val wellFormed = reasons.isEmpty
    SchemaInfo(name, "Iterative", wellFormed, reasons)
  }

}

object ShExSchema {
  def empty: ShExSchema = ShExSchema(schema = Schema_.empty)

  def fromString(
    cs: CharSequence,
    format: String,
    base: Option[String]): Either[String, ShExSchema] = {
    Schema_.fromString(cs, format, base, RDFAsJenaModel.empty).map(p => ShExSchema(p))
  }

}
