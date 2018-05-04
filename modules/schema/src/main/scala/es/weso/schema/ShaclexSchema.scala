package es.weso.schema
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.{Schema => ShaclSchema, _}
import es.weso.shacl._
import es.weso.shacl.converter.{RDF2Shacl, Shacl2ShEx}
import es.weso.shacl.report.{ValidationReport, ValidationResult}
import es.weso.shacl.validator.{CheckResult, Evidence, ShapeTyping, Validator}
import es.weso.shapeMaps._
import es.weso.shex.Schema

import util._
import es.weso.typing._
import es.weso.utils.MapUtils

case class ShaclexSchema(schema: ShaclSchema) extends Schema {
  override def name = "SHACLex"

  override def formats = DataFormats.formatNames ++ Seq("TREE")

  override def defaultTriggerMode = TargetDeclarations

  override def validate(rdf: RDFReader, trigger: ValidationTrigger): Result = trigger match {
    case TargetDeclarations => validateTargetDecls(rdf).addTrigger(trigger)
    case _ => Result.errStr(s"Not implemented trigger ${trigger.name} for SHACL yet")
  }

  def validateTargetDecls(rdf: RDFReader): Result = {
    val validator = Validator(schema)
    val r = validator.validateAll(rdf)
    val builder = RDFAsJenaModel.empty
    builder.addPrefixMap(schema.pm)
    cnvResult(r, rdf, builder)
  }

  def cnvResult(
                 r: CheckResult[ValidationResult, ShapeTyping, List[Evidence]],
                 rdf: RDFReader,
                 builder: RDFBuilder
               ): Result = {
    val vr: ValidationReport =
      r.result.fold(e => ValidationReport.fromError(e),
        _.toValidationReport
      )
    Result(
      isValid = r.isOK,
      message = if (r.isOK) "Valid" else "Not valid",
      shapeMaps = r.results.map(cnvShapeTyping(_, rdf)),
      validationReport = vr.toRDF(builder),
      errors = r.errors.map(cnvViolationError(_)),
      trigger = None,
      nodesPrefixMap = rdf.getPrefixMap(),
      shapesPrefixMap = schema.pm)
  }

  def cnvShapeTyping(t: ShapeTyping, rdf: RDFReader): ResultShapeMap = {
    ResultShapeMap(
      t.getMap.mapValues(cnvMapShapeResult), rdf.getPrefixMap(), schema.pm)
  }

  private def cnvMapShapeResult(m: Map[Shape, TypingResult[ValidationResult, String]]): Map[ShapeMapLabel, Info] = {

    MapUtils.cnvMap(m, cnvShape, cnvTypingResult)
  }

  private def cnvShape(s: Shape): ShapeMapLabel = {
    s.id match {
      case iri: IRI => IRILabel(iri)
      case bnode: BNode => BNodeLabel(bnode)
      case _ => throw new Exception(s"cnvShape: unexpected ${s.id}")
    }
  }

  private def cnvTypingResult(t: TypingResult[ValidationResult, String]): Info = {
    import showShacl._
    import TypingResult.showTypingResult
    Info(
      status = if (t.isOK) Conformant else NonConformant,
      reason = Some(t.show)
    // TODO: Convert typing result to JSON and add it to appInfo
    )
  }

  private def cnvViolationError(v: ValidationResult): ErrorInfo = {
    val pm = schema.pm
    ErrorInfo(
      pm.qualify(v.sourceConstraintComponent) +
        " FocusNode: " + schema.pm.qualify(v.focusNode) + " " +
        v.message.mkString(","))
  }

  /*def validateShapeMap(sm: Map[RDFNode,Set[String]], nodesStart: Set[RDFNode], rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented validateShapeMap")
  }*/

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Either[String, Schema] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs, format, base)
      schema <- RDF2Shacl.getShacl(rdf)
    } yield ShaclexSchema(schema)
  }

  override def fromRDF(rdf: RDFReader): Either[String, Schema] = for {
    schemaShacl <- RDF2Shacl.getShacl(rdf)
  } yield ShaclexSchema(schemaShacl)

  override def serialize(format: String): Either[String, String] = {
    val builder: RDFBuilder = RDFAsJenaModel.empty
    if (formats.contains(format.toUpperCase))
      schema.serialize(format, builder)
    else
      Left(s"Format $format not supported to serialize $name. Supported formats=$formats")
  }

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).map(_.toString).toList
  }

  override def pm: PrefixMap = schema.pm

  def convert(targetFormat: Option[String], targetEngine: Option[String]): Either[String,String] = {
   targetEngine.map(_.toUpperCase) match {
     case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some(engine) if (engine.equalsIgnoreCase(name)) => {
       serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     }
     case Some("SHEX") => for {
       newSchema <- Shacl2ShEx.shacl2ShEx(schema).toEither.leftMap(es => es.toList.mkString("\n"))
       builder = RDFAsJenaModel.empty
       str <- Schema.serialize(newSchema,targetFormat.getOrElse(DataFormats.defaultFormatName),builder)
     } yield str
     case Some(other) => Left(s"Conversion $name -> $other not implemented yet")
   }
  }

  override def info: SchemaInfo = {
    // TODO: Check if shacl schemas are well formed
    SchemaInfo(name,"SHACLex", true, List())
  }
}

object ShaclexSchema {
  def empty: ShaclexSchema = ShaclexSchema(schema = ShaclSchema.empty)


}
