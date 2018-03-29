package es.weso.schema
import cats._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.{Schema => ShaclSchema, _}
import es.weso.shacl._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.{CheckResult, Evidence, Validator, ViolationError, ShapeTyping}
import es.weso.shapeMaps._
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
    cnvResult(r, rdf)
  }

  def cnvResult(
    r: CheckResult[ViolationError, ShapeTyping, List[Evidence]],
    rdf: RDFReader): Result =
    Result(
      isValid = r.isOK,
      message = if (r.isOK) "Valid" else "Not valid",
      shapeMaps = r.results.map(cnvShapeTyping(_, rdf)),
      validationReport = None,
      errors = r.errors.map(cnvViolationError(_)),
      trigger = None,
      nodesPrefixMap = rdf.getPrefixMap(),
      shapesPrefixMap = schema.pm)

  def cnvShapeTyping(t: ShapeTyping, rdf: RDFReader): ResultShapeMap = {
    ResultShapeMap(
      t.getMap.mapValues(cnvMapShapeResult), rdf.getPrefixMap(), schema.pm)
  }

  private def cnvMapShapeResult(m: Map[Shape, TypingResult[ViolationError, String]]): Map[ShapeMapLabel, Info] = {

    MapUtils.cnvMap(m, cnvShape, cnvTypingResult)
  }

  private def cnvShape(s: Shape): ShapeMapLabel = {
    s.id match {
      case iri: IRI => IRILabel(iri)
      case bnode: BNode => BNodeLabel(bnode)
      case _ => throw new Exception(s"cnvShape: unexpected ${s.id}")
    }
  }

  private def cnvTypingResult(t: TypingResult[ViolationError, String]): Info = {
    import showShacl._
    import TypingResult.showTypingResult
    val showVE = implicitly[Show[ViolationError]]
    val x = implicitly[Show[TypingResult[ViolationError, String]]]
    Info(
      status = if (t.isOK) Conformant else NonConformant,
      reason = Some(x.show(t))
    // TODO: Convert typing result to JSON and add it to appInfo
    )
  }

  private def cnvViolationError(v: ViolationError): ErrorInfo = {
    val pm = schema.pm
    ErrorInfo(
      pm.qualify(v.sourceConstraintComponent) +
        " FocusNode: " + schema.pm.qualify(v.focusNode) + " " +
        v.message.getOrElse(""))
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
    if (formats.contains(format))
      schema.serialize(format)
    else
      Left(s"Format $format not supported to serialize $name. Supported formats=$formats")
  }

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).map(_.toString).toList
  }

  override def pm: PrefixMap = schema.pm

  def convert(targetFormat: Option[String], targetEngine: Option[String]): Either[String,String] = {
   targetEngine match {
     case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some(engine) if (engine.equalsIgnoreCase(name)) => {
       serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     }
     case Some(other) =>
      Left(s"Conversion between Schema engines not implemented yet")
   }
  }

}

object ShaclexSchema {
  def empty: ShaclexSchema = ShaclexSchema(schema = ShaclSchema.empty)


}
