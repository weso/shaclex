package es.weso.shacl

import es.weso.rdf.nodes._
import SHACLPrefixes._
import es.weso.rdf.path.{PredicatePath, SHACLPath}

case class ViolationError(
    id: IRI,
    focusNode: RDFNode,
    subject: Option[RDFNode],
    path: Option[SHACLPath],
    obj: Option[RDFNode],
    message: Option[String],
    sourceConstraint: Option[RDFNode]) {
  override def toString = s"Violation error on $focusNode: ${message.getOrElse("")}"
}

object ViolationError {

  def basic(suffix: String, focusNode: RDFNode, attempt: Attempt, msg: String) =
    ViolationError(id = sh + suffix,
      focusNode = focusNode,
      subject = None,
      path = attempt.path,
      obj = None,
      message = Some(msg + s" Node: ${attempt.node}, Shape: ${attempt.shapeIRI.getOrElse(IRI(""))}, path: ${attempt.path.getOrElse(PredicatePath(IRI("")))}"),
      sourceConstraint = attempt.shapeIRI)

  def failedNodeShape(node: RDFNode, shape: NodeShape, attempt: Attempt, msg: String) =
    basic("FailedNodeShape", node, attempt, msg)

  def classError(focusNode: RDFNode, cls: RDFNode, attempt: Attempt) =
    basic("classError", focusNode, attempt, s"Node $focusNode doesn't belong to class $cls")

  def datatypeError(focusNode: RDFNode, datatype: RDFNode, attempt: Attempt) =
    basic("dataTypeError", focusNode, attempt, s"Node $focusNode doesn't have dataType $datatype")

  def unsupported(focusNode: RDFNode, attempt: Attempt, msg: String) =
    basic("unsupported", focusNode, attempt, "Unsupported: " + msg)

  def notNumeric(focusNode: RDFNode, attempt: Attempt) =
    basic("notNumericError", focusNode, attempt, s"NotNumeric violation. Expected $focusNode to be a number")

  def minExclusiveError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("minExclusiveError", focusNode, attempt, s"minExclusive violation. Expected $focusNode > $n")

  def minInclusiveError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("minInclusiveError", focusNode, attempt, s"minInclusive violation. Expected $focusNode >= $n")

  def maxExclusiveError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("maxExclusiveError", focusNode, attempt, s"maxExclusive violation. Expected $focusNode < $n")

  def maxInclusiveError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("maxInclusiveError", focusNode, attempt, s"maxInclusive violation. Expected $focusNode <= $n")

  def minLengthError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("minLengthError", focusNode, attempt, s"minLength violation. Expected length($focusNode) >= $n")

  def maxLengthError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("maxLengthError", focusNode, attempt, s"maxLength violation. Expected length($focusNode) <= $n")

  def patternError(focusNode: RDFNode, attempt: Attempt, p: String, flags: Option[String]) =
    basic("patternError", focusNode, attempt, s"pattern violation. Expected $focusNode to match '$p'${flags.getOrElse("")}")

  def uniqueLangError(focusNode: RDFNode, attempt: Attempt, path: SHACLPath, vs: Seq[RDFNode]) =
    basic("uniqueLangError", focusNode, attempt, s"uniqueLang violation. Expected $focusNode to have a unique language for path $path with values $vs")

  def languageInError(focusNode: RDFNode, attempt: Attempt, langs: List[String]) =
    basic("languageInError", focusNode, attempt, s"languageIn violation. Expected $focusNode to match 'languageIn(${langs.mkString(",")})'")

  def equalsError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("equals", focusNode, attempt, p, vs)
    
  def disjointError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("disjoint", focusNode, attempt, p, vs)
    
  def lessThanError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("lessThan", focusNode, attempt, p, vs)
    
  def lessThanOrEqualsError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("lessThanOrEquals", focusNode, attempt, p, vs)

  def comparisonError(name: String, focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    basic(s"${name}Error", focusNode, attempt, s"$name violation. Expected $focusNode to match $name '$p', values: $vs")
    
  def minCountError(focusNode: RDFNode, attempt: Attempt, minCount: Int, count: Int) =
    basic("minCountError", focusNode, attempt, s"MinCount violation. Expected $minCount, obtained: $count")

  def maxCountError(focusNode: RDFNode, attempt: Attempt, maxCount: Int, count: Int) =
    basic("maxCountError", focusNode, attempt, s"MaxCount violation. Expected $maxCount, obtained: $count")

  def iriKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("iriKindError", focusNode, attempt, s"Node $focusNode is not an IRI")

  def literalKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("literalKindError", focusNode, attempt, s"Node $focusNode is not a Literal")

  def bNodeKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("bNodeKindError", focusNode, attempt, s"Node $focusNode is not a blank node")

  def bNodeOrIRIKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("bNodeOrIRIKindError", focusNode, attempt, s"Node $focusNode is not a blank node or an IRI")

  def bNodeOrLiteralKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("bNodeOrLiteralKindError", focusNode, attempt, s"Node $focusNode is not a blank node or a Literal")

  def iriOrLiteralKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("iriOrLiteralKindError", focusNode, attempt, s"Node $focusNode is not a IRI or a Literal")

  def notError(focusNode: RDFNode, attempt: Attempt, shape: NodeShape) =
    basic("notError", focusNode, attempt, s"Not violation. Expected $focusNode not to satisfy ${shape.showId}")

  def andError(focusNode: RDFNode, attempt: Attempt, shapes: List[NodeShape]) =
    basic("andError", focusNode, attempt, s"And violation. Expected $focusNode to satisfy all of the shapes ${shapes.map(_.showId).mkString(",")}")

  def orError(focusNode: RDFNode, attempt: Attempt, shapes: List[NodeShape]) =
    basic("orError", focusNode, attempt, s"Or violation. Expected $focusNode to satisfy some of the shapes ${shapes.map(_.showId).mkString(",")}")

  def hasValueError(focusNode: RDFNode, attempt: Attempt, value: Value) =
    basic("hasValueError", focusNode, attempt, s"HasValue error. Expected $focusNode to be  $value")
    
  def inError(focusNode: RDFNode, attempt: Attempt, values: Seq[Value]) =
    basic("inError", focusNode, attempt, s"In violation. Expected $focusNode to be in $values")

  def closedError(
      focusNode: RDFNode, 
      attempt: Attempt, 
      allowedProperties: List[IRI],
      ignoredProperties: List[IRI],
      notAllowed: List[IRI]) =
    basic("closedError", focusNode, attempt, 
        s"closed violation. $focusNode has more properties than $allowedProperties. Extra properties found: $notAllowed, ignoredProperties: $ignoredProperties")
}
