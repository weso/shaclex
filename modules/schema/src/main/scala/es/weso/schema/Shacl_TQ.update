package es.weso.schema

import es.weso.shacl_tq.{ViolationError => _, ShaclBinder}
import es.weso.schema.shacl_tq._
import es.weso.shex.DataFormat
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import es.weso.validating._
import util._
import org.apache.jena.rdf.model.Model
import es.weso.validating.Checked._
import es.weso.rdf.PREFIXES.{rdf_type}
import es.weso.utils.TryUtils
import es.weso.rdf.PrefixMap

case class Shacl_TQ(
    binder: ShaclBinder) extends Schema {

  import Shacl_TQ._

  override def name = "SHACL_TQ"
  
  override def formats = DataFormat.formatNames
  
  override def toHTML(format: String): String = {
    "<pre>" + binder.serialize(format) + "</pre>"
  }
  
  override def validate(rdf: RDFReader) : Result = {
    val result: Model = binder.validateModel(rdf)
    val checked: Checked[Boolean,ConstraintReason,ViolationError] = convertResultModel(result)
    checked2Result(checked)
  }
  
  def convertResultModel(resultModel: Model): Checked[Boolean,ConstraintReason,ViolationError] = {
    if (resultModel.size == 0) ok(SingleReason(true,"Validated"))
    else {
      val result = RDFAsJenaModel(resultModel)
      val ts = result.triplesWithPredicateObject(rdf_type,sh_ValidationResult)
      val vs = ts.map(_.subj).map(s => getViolationError(result,s)).toSeq
      val violationErrors = TryUtils.filterSuccess(vs)
      violationErrors match {
        case Success(Seq()) => {
          checkError(ViolationError.msgError(s"Error but not validationResults is empty?"))
        }
        case Success(es) => errs(es)
        case Failure(e) => 
          checkError(ViolationError.msgError(e.getMessage))
      }
    }
  }
  
  def showResultModel(result:RDFReader): String = {
    result.serialize("TURTLE")
  }
  
  private def getViolationError(result: RDFReader, node: RDFNode): Try[ViolationError] = {
    val ts = result.triplesWithSubjectPredicate(node,sh_message)
    val msg = 
      if (ts.size == 1) ts.head.obj.toString
      else "<not found message>"
    ViolationError.parse(result,node)
  }

  override def validateNodeShape(node: IRI, shape: String, rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateNodesLabels for SHACL TQ")
  }
  
  override def validateNodeAllShapes(node: IRI, rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateAllNodesAllLabels for SHACL TQ")
  }
  
  override def validateAllNodesAllShapes(rdf: RDFReader) : Result = {
    throw new Exception("Not implemented validateAllNodesAllLabels for SHACL TQ")
  }
  
  def checked2Result(result: Checked[Boolean,ConstraintReason,ViolationError]): Result = {
    val isValid = result.isOK
    val msg = 
      if (result.isOK) s"No errors found"
      else s"Not Valid" 
    val solutions: Seq[Solution] = Seq()
    val errors: Seq[ErrorInfo] = result.errors.map(violationError2ErrorInfo(_))
    Result(isValid,msg,solutions,errors)
  }

  def errResult(msg: String): Result = {
    Result(false,msg,Seq(),Seq())  
  }
  
  def violationError2ErrorInfo(ve: ViolationError): ErrorInfo = {
    ErrorInfo(ve.toHTMLRow(pm))
  }
  
  override def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema] = {
    val b : ShaclBinder = binder.fromString(cs,format,base)
    val s : Schema = Shacl_TQ(b)  
    Success(s)
  }
  
  override def fromRDF(rdf: RDFReader): Try[Schema] = {
    val b: ShaclBinder = ShaclBinder.fromRDF(rdf)
    val s : Schema = Shacl_TQ(b)  
    Success(s)
  }
  
  override def serialize(format: String): Try[String] = {
    Success(binder.serialize(format)) 
  }
  
  override def empty: Schema = Shacl_TQ.empty
  
  override def shapes: List[String] = binder.shapes.map(_.toString)
  
  override def pm: PrefixMap = binder.pm
  
  override def htmlBeforeErrors = 
    "<thead><tr><th>Message</th><th>Focus Node</th><th>Subject</th><th>Predicate</th><th>Severity</th><th>Source constraint</th><th>Source Shape</th><th>Source template</th></tr></thead><tbody>"

  override def htmlAfterErrors = 
    "</tbody>"
  
  override def htmlBeforeSolutions = 
    ""

  override def htmlAfterSolutions = 
    ""

}

object Shacl_TQ {
  def empty: Shacl_TQ = Shacl_TQ(binder = ShaclBinder.empty)
  
  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Shacl_TQ] = { 
    val b : ShaclBinder = ShaclBinder.fromString(cs,format,base)
    val s = Shacl_TQ(b)  
    Success(s)
  }

  lazy val sh = IRI("http://www.w3.org/ns/shacl#")
  lazy val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  
  lazy val sh_ValidationResult = sh + "ValidationResult"
  lazy val sh_message = sh + "message"

  lazy val PMShacl: PrefixMap = PrefixMap.empty.
        addPrefix("sh",sh).
        addPrefix("xsd",xsd).
        addPrefix("rdf",rdf).
        addPrefix("rdfs",rdfs)

}
