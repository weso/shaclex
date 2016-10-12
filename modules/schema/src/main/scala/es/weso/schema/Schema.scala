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

 def validate(rdf: RDFReader): Result

 def validateNodeShape(node: IRI, label: String, rdf: RDFReader): Result

 def validateNodeAllShapes(node: IRI, rdf: RDFReader): Result

 def validateAllNodesAllShapes(rdf: RDFReader): Result

 def fromString(cs: CharSequence, format: String, base: Option[String]): Try[Schema]

 def fromRDF(rdf: RDFReader): Try[Schema]

 def serialize(format: String): Try[String]

 def defaultFormat: String = formats.head

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
