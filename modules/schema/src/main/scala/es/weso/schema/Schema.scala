package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import util._

abstract class Schema {

  /**
   * Name of this schema. Example: ShEx, Shacl_TQ, ...
   */
  def name: String

  /**
   * Supported input formats
   */
  def formats: Seq[String]

  def validate(rdf: RDFReader, trigger: ValidationTrigger): Result /*= {
  trigger match {
   case TargetDeclarations => validateTargetDecls(rdf)
   case ShapeMapTrigger(sm,nodes) => validateShapeMap(sm,nodes,rdf)
   case _ => throw new Exception(s"Unsupported validation trigger $trigger")
  }
 } */

  def validate(
    rdf: RDFReader,
    triggerMode: String,
    shapeMap: String,
    optNode: Option[String],
    optShape: Option[String],
    nodePrefixMap: PrefixMap = PrefixMap.empty,
    shapesPrefixMap: PrefixMap = pm): Result = {
    ValidationTrigger.findTrigger(triggerMode, shapeMap, optNode, optShape, nodePrefixMap, shapesPrefixMap) match {
      case Left(err) =>
        Result.errStr(s"Cannot get trigger: $err. TriggerMode: $triggerMode, prefixMap: $pm")
      case Right(trigger) =>
        validate(rdf, trigger)
    }
  }

  // def validateTargetDecls(rdf: RDFReader): Result

  // def validateNodeShape(node: IRI, label: String, rdf: RDFReader): Result

  // def validateNodeStart(node: IRI, rdf: RDFReader): Result

  // def validateShapeMap(map: Map[RDFNode,Set[String]], nodesStart: Set[RDFNode], rdf: RDFReader): Result

  def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema]

  def fromRDF(rdf: RDFReader): Try[Schema]

  def serialize(format: String): Try[String]

  def defaultFormat: String = formats.head

  def defaultTriggerMode: ValidationTrigger

  /**
   * Creates an empty schema
   */
  def empty: Schema

  /**
   * List of shapes
   */
  def shapes: List[String]

  /**
   * Prefix Map of this schema
   */
  def pm: PrefixMap

  /**
   * String to add to HTML conversion of validating solution
   */
  // def htmlBeforeErrors: String = ""

  // def htmlAfterErrors: String = ""

  /**
   * String to add to HTML conversion of validation errors
   */
  // def htmlBeforeSolutions: String = ""

  // def htmlAfterSolutions: String = ""
}
