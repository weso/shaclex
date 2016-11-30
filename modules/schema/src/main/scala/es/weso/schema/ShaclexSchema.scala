package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.{Schema => ShaclSchema, _}
import es.weso.shacl.Validator._
import es.weso.shacl._
import util._
import es.weso.typing._

import scala.util.{Failure, Try}

case class ShaclexSchema(schema: ShaclSchema) extends Schema {
  override def name = "SHACLex"

  override def formats = DataFormats.formatNames

  override def validateTargetDecls(rdf: RDFReader) : Result = {
    val validator = Validator(schema)
    val r = validator.validateAll(rdf)
    cnvResult(r,rdf)
  }

  def cnvResult(r:CheckResult[ViolationError, ShapeTyping, List[(es.weso.shacl.NodeShape,String)]],
                rdf: RDFReader): Result =
    Result(isValid = r.isOK,
           message = if (r.isOK) "Valid" else "Not valid",
           solutions = r.results.map(cnvShapeTyping(_,rdf)),
           errors = r.errors.map(cnvViolationError(_))
    )

  def cnvShapeTyping(t: ShapeTyping, rdf: RDFReader): Solution = {
    Solution(t.getMap.mapValues(cnvResult),
      rdf.getPrefixMap(),
      schema.pm)
  }

  def cnvResult(
    r: Map[Shape,TypingResult[ViolationError,String]]
  ): InfoNode = {
    val (oks,bads) = r.toSeq.partition(_._2.isOK)
    InfoNode(oks.map(cnvShapeResult(_)),
             bads.map(cnvShapeResult(_)),
      schema.pm
    )
  }

    def cnvShapeResult(
      p:(Shape,TypingResult[ViolationError,String])
    ): (SchemaLabel,Explanation) = {
      val shapeLabel = p._1.id match {
        case Some(iri) => SchemaLabel(schema.pm.qualify(iri),schema.pm)
        case None => SchemaLabel("<Unknown label>",schema.pm)
      }
      val explanation = Explanation(cnvTypingResult(p._2))
    (shapeLabel,explanation)
    }

  def cnvTypingResult(result: TypingResult[ViolationError,String]): String = {
    result.t.fold(
      es => "Errors: " +
           es.map(_.message.getOrElse("")).toList.mkString("\n")
    , rs => "OK. Evidences:" +
           rs.map(" " + _).mkString("\n")
    )
  }

  def cnvViolationError(v: ViolationError): ErrorInfo = {
    val pm = schema.pm
    ErrorInfo(
      pm.qualify(v.id) +
        " FocusNode: " + schema.pm.qualify(v.focusNode) + " " +
        v.message.getOrElse("")
    )
  }

  override def validateNodeShape(node: IRI, shape: String, rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateNodesShape for SHACLex yet")
  }

  override def validateNodeStart(node: IRI, rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented nodeStart")
  }

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs,format,base)
      schema <- RDF2Shacl.getShacl(rdf)
    } yield ShaclexSchema(schema)
  }

  override def fromRDF(rdf: RDFReader): Try[Schema] = {
    for {
      schema <- RDF2Shacl.getShacl(rdf)
    } yield ShaclexSchema(schema)
  }

  override def serialize(format: String): Try[String] = {
    if (formats.contains(format))
      schema.serialize(format)
    else
      Failure(new Exception(s"Format $format not supported to serialize $name. Supported formats=$formats"))
  }

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).filter(_.isDefined).map(_.get).map(_.toString).toList
  }

  override def pm: PrefixMap = PrefixMap.empty // TODO: Improve this to add pm to Shaclex
}

object ShaclexSchema {
  def empty: ShaclexSchema = ShaclexSchema(schema = ShaclSchema.empty)

  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[ShaclexSchema] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs,format,base)
      schema <- RDF2Shacl.getShacl(rdf)
    } yield ShaclexSchema(schema)
  }

}
