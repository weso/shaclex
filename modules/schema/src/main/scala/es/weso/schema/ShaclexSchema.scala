package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.{ Schema => ShaclSchema, Shape => ShaclNodeShape, _ }
import es.weso.shacl.Validator._
import es.weso.shacl._
import es.weso.shacl.converter.RDF2Shacl

import util._
import es.weso.typing._

import scala.util.{ Failure, Success, Try }

case class ShaclexSchema(schema: ShaclSchema) extends Schema {
  override def name = "SHACLex"

  override def formats = DataFormats.formatNames ++ Seq("TREE")

  override def defaultTriggerMode = TargetDeclarations

  override def validate(rdf: RDFReader, trigger: ValidationTrigger): Result = trigger match {
    case TargetDeclarations => validateTargetDecls(rdf).addTrigger(trigger)
    case MapTrigger(sm, ts) => Result.errStr("Not implemented ShapeMap for SHACL yet")
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
      solutions = r.results.map(cnvShapeTyping(_, rdf)),
      errors = r.errors.map(cnvViolationError(_)),
      trigger = None)

  def cnvShapeTyping(t: ShapeTyping, rdf: RDFReader): Solution = {
    Solution(
      t.getMap.mapValues(cnvResultNodeShape),
      rdf.getPrefixMap(),
      schema.pm)
  }

  def cnvResultNodeShape(
    r: Map[ShaclNodeShape, TypingResult[ViolationError, String]]): InfoNode = {
    val (oks, bads) = r.toSeq.partition(_._2.isOK)
    InfoNode(
      oks.map(cnvShapeResult(_)),
      bads.map(cnvShapeResult(_)),
      schema.pm)
  }

  def cnvShapeResult(
    p: (ShaclNodeShape, TypingResult[ViolationError, String])): (SchemaLabel, Explanation) = {
    val shapeLabel = SchemaLabel(p._1.showId)
    val explanation = Explanation(cnvTypingResult(p._2))
    (shapeLabel, explanation)
  }

  def cnvTypingResult(result: TypingResult[ViolationError, String]): String = {
    result.t.fold(
      es => "Errors: " +
        es.map(_.message.getOrElse("")).toList.mkString("\n"), rs => "OK. Evidences:" +
        rs.map(" " + _).mkString("\n"))
  }

  def cnvViolationError(v: ViolationError): ErrorInfo = {
    val pm = schema.pm
    ErrorInfo(
      pm.qualify(v.id) +
        " FocusNode: " + schema.pm.qualify(v.focusNode) + " " +
        v.message.getOrElse(""))
  }

  /*def validateShapeMap(sm: Map[RDFNode,Set[String]], nodesStart: Set[RDFNode], rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented validateShapeMap")
  }*/

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs, format, base)
      schema <- tryGetShacl(rdf)
    } yield ShaclexSchema(schema)
  }

  def tryGetShacl(rdf: RDFReader) =
    RDF2Shacl.getShacl(rdf).fold(
      s =>
        Failure(new Exception(s)),
      Success(_))

  override def fromRDF(rdf: RDFReader): Try[Schema] = for {
    schemaShacl <- tryGetShacl(rdf)
  } yield ShaclexSchema(schemaShacl)

  override def serialize(format: String): Try[String] = {
    if (formats.contains(format))
      schema.serialize(format)
    else
      Failure(new Exception(s"Format $format not supported to serialize $name. Supported formats=$formats"))
  }

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).map(_.toString).toList
  }

  override def pm: PrefixMap = PrefixMap.empty // TODO: Improve this to add pm to Shaclex
}

object ShaclexSchema {
  def empty: ShaclexSchema = ShaclexSchema(schema = ShaclSchema.empty)

  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[ShaclexSchema] = format match {
    case "TREE" => Failure(new Exception(s"Not implemented reading from format $format yet"))
    case _ => for {
      rdf <- RDFAsJenaModel.fromChars(cs, format, base)
      schema <- RDF2Shacl.getShacl(rdf) match {
        case Left(s) => Failure(new Exception(s))
        case Right(s) => Success(s)
      }
    } yield ShaclexSchema(schema)
  }

}
