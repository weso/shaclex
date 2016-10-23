package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.{Schema => ShExSchema, _}
import es.weso.shex.validator._
import es.weso.shex._
import util._
import es.weso.typing._

case class ShEx(schema: ShExSchema) extends Schema {
  override def name = "ShEx"

  override def formats =
    List("SHEXC","SHEXJ") ++
    RDFAsJenaModel.availableFormats

  override def validate(rdf: RDFReader) : Result = {
    val validator = Validator(schema)
    val r = validator.validateAll(rdf)
    cnvResult(r)
  }

  def cnvResult(
    r: CheckResult[ViolationError, ShapeTyping,
                   List[(es.weso.shex.validator.NodeShape,String)]]
  ): Result =
    Result(isValid = r.isOK,
           message = if (r.isOK) "Valid" else "Not valid",
           solutions = r.results.map(cnvShapeTyping(_)),
           errors = r.errors.map(cnvViolationError(_))
    )

  def cnvShapeTyping(st: ShapeTyping): Solution = {
    Solution(st.t.getMap.mapValues(cnvResult))
  }

  def cnvResult(
    r: Map[ShapeType,TypingResult[ViolationError,String]]
  ): InfoNode = {
    val (oks,bads) = r.toSeq.partition(_._2.isOK)
    InfoNode(oks.map(cnvShapeResult(_)),
             bads.map(cnvShapeResult(_))
    )
  }

    def cnvShapeResult(
      p:(ShapeType,TypingResult[ViolationError,String])
    ): (ShapeLabel,Explanation) = {
      val shapeLabel = p._1.label match {
        case Some(IRILabel(iri)) => ShapeLabel(schema.prefixMap.qualify(iri))
        case Some(BNodeLabel(bn)) => ShapeLabel(bn.id)
        case None => ShapeLabel(s"Anonymous shape ${p._1.shape}")
      }
      val explanation = Explanation(cnvTypingResult(p._2))
    (shapeLabel,explanation)
    }

  def cnvTypingResult(result: TypingResult[ViolationError,String]): String = {
    result.t.fold(
      es => "Errors: " +
           es.toList.mkString("\n")
    , rs => "OK. Evidences:" +
           rs.map(" " + _).mkString("\n")
    )
  }

  def cnvViolationError(v: ViolationError): ErrorInfo = {
    ErrorInfo(v.msg)
  }

  override def validateNodeShape(node: IRI, shape: String, rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented validateNodeShape")
/*    val matcher = ShExMatcher(schema,rdf)
    val pm = schema.pm
    val maybeLabel = pm.qname(shape).map(lbl => ShExLabel.mkLabel(lbl)).flatten
    maybeLabel match {
      case Some(lbl) => {
        val r = matcher.match_node_label(node)(lbl)
        validationResult2Result(r)
      }
      case None => Result.errStr(s"Cannot make label from shape $shape")
    } */
  }

  override def validateNodeAllShapes(node: IRI, rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented validateNodeAllShapes")
/*    val matcher = ShExMatcher(schema,rdf)
    val r = matcher.match_node_AllLabels(node)
    validationResult2Result(r) */
  }

  override def validateAllNodesAllShapes(rdf: RDFReader) : Result = {
        throw new Exception("Unimplemented validateNodeAllShapes")
/*    val matcher = ShExMatcher(schema,rdf)
    val r = matcher.matchAllNodes_AllLabels
    validationResult2Result(r) */
  }

/*  def hasSolutions(rs: Seq[Map[RDFNode,(Seq[ShExLabel],Seq[ShExLabel])]]): Boolean = {
    if (rs.size == 0) false
    else if (rs.size == 1 && rs.head.isEmpty) false
    else true
  }

  def validationResult2Result(result: ValidationResult[RDFNode,ShExLabel,Throwable]): Result = {
    val isValid = result.isValid
    val (msg,solutions,errors): (String,Seq[Solution],Seq[ErrorInfo]) = {
      result.extract match {
        case Success(rs) => {
          if (hasSolutions(rs))
            ("Solutions found", rs.map(cnvSol(_)), Seq())
          else
            ("No Results", Seq(), Seq(ErrorInfo("No results")))
        }
        case Failure(e) => (s"Error $e.getMessage", Seq(), Seq(ErrorInfo(e.getMessage)))
      }
    }
    val r = Result(isValid, msg, solutions, errors)
    println(s"validationresult2Result result: $result, r: $r")
    r
  }

  def cnvSol(rs: Map[RDFNode, (Seq[ShExLabel], Seq[ShExLabel])]): Solution = {
    Solution(rs.mapValues(cnvShapes(_)))
  }

  def cnvShapes(pair: (Seq[ShExLabel], Seq[ShExLabel])): InfoNode = {
    val (shapes,noShapes) = pair
    InfoNode(shapes.map(mkLabelExplanation(_)),noShapes.map(mkLabelExplanation(_)))
  }

  def mkLabelExplanation(lbl: ShExLabel): (ShapeLabel,Explanation) = {
    (ShapeLabel(lbl.toString),Explanation(""))
  } */

  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema] = {
    ShEx.fromString(cs,format,base)
  }

  override def fromRDF(rdf: RDFReader): Try[Schema] =
    Failure(new Exception("Not implemented get ShEx from RDF yet"))
  /*{
    for {
      schema <- RDF2Schema.rdf2Schema(rdf)
    } yield ShEx(schema)
  }*/

  override def serialize(format: String): Try[String] = {
    if (formats.contains(format.toUpperCase()))
      Success(ShExSchema.serialize(schema, format))
    else
      Failure(
        new Exception(
          s"Can't serialize to format $format. Supported formats=$formats"))
  }

  override def empty: Schema = ShEx.empty

  override def shapes: List[String] = {
    val pm = schema.prefixMap
    schema.labels.map(_.qualifiedShow(pm))
  }

  override def pm: PrefixMap = schema.prefixMap

}

object ShEx {
  def empty: ShEx = ShEx(schema = ShExSchema.empty)

  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[ShEx] = {
    ShExSchema.fromString(cs,format,base).map(p => ShEx(p))
  }

}
