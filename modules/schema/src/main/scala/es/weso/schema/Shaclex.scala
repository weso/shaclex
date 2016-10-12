package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.{Schema => ShaclSchema, _}
import es.weso.shacl.Validator._
import es.weso.shacl._
import util._
import es.weso.typing._

case class Shaclex(schema: ShaclSchema) extends Schema {
  override def name = "SHACLex"

  override def formats = RDFAsJenaModel.availableFormats

  override def validate(rdf: RDFReader) : Result = {
    val validator = Validator(schema)
    val r = validator.validateAll(rdf)
    cnvResult(r)
  }

  def cnvResult(r: CheckResult[ViolationError, ShapeTyping, List[(es.weso.shacl.NodeShape,String)]]): Result =
    Result(isValid = r.isOK,
           message="",
           solutions=r.results.map(cnvShapeTyping(_)),
           errors=r.errors.map(cnvViolationError(_))
    )

  def cnvShapeTyping(t: ShapeTyping): Solution = {
    Solution(t.getMap.mapValues(cnvResult))
  }

  def cnvResult(
    r: Map[Shape,TypingResult[ViolationError,String]]
  ): InfoNode = {
    val (oks,bads) = r.toSeq.partition(_._2.isOK)
    def cnvShapeResult(
      p:(Shape,TypingResult[ViolationError,String])
    ): (ShapeLabel,Explanation) = {
      val shapeLabel = p._1.id match {
        case Some(iri) => schema.pm.qualify(iri)
        case None => throw new Exception("Unimplemented cnvResult with None")
      }
      ???
    }

    InfoNode(oks.map(cnvShapeResult(_)),
             bads.map(cnvShapeResult(_))
    )
  }


  def cnvViolationError(v: ViolationError): ErrorInfo = ???

  override def validateNodeShape(node: IRI, shape: String, rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateNodesShape for SHACLex yet")
  }

  override def validateNodeAllShapes(node: IRI, rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateNodesAllShapes for SHACLex yet")
  }

  override def validateAllNodesAllShapes(rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateAllNodesAllShapes for SHACL yet")
  }

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs,format,base)
      schema <- RDF2Shacl.getShacl(rdf)
    } yield Shaclex(schema)
  }

  override def fromRDF(rdf: RDFReader): Try[Schema] = {
    for {
      schema <- RDF2Shacl.getShacl(rdf)
    } yield Shaclex(schema)
  }

  override def serialize(format: String): Try[String] = {
    if (formats.contains(format))
      schema.serialize(format)
    else
      Failure(new Exception(s"Format $format not supported to serialize $name. Supported formats=$formats"))
  }

  override def empty: Schema = Shaclex.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).filter(_.isDefined).map(_.get).map(_.toString).toList
  }

  override def pm: PrefixMap = PrefixMap.empty // TODO: Improve this to add pm to Shaclex
}

object Shaclex {
  def empty: Shaclex = Shaclex(schema = ShaclSchema.empty)

  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Shaclex] = {
    for {
      rdf <- RDFAsJenaModel.fromChars(cs,format,base)
      schema <- RDF2Shacl.getShacl(rdf)
    } yield Shaclex(schema)
  }

}
