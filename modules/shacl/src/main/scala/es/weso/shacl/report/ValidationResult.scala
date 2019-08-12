package es.weso.shacl.report

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES.sh
import es.weso.rdf.path._
import es.weso.shacl._
import es.weso.shacl.validator.Attempt


// TODO: Refactor this code creating Classes for each error?

abstract class AbstractResult

case class ValidationResult(focusNode: RDFNode,
                       resultSeverity: Severity,
                       sourceConstraintComponent: IRI,
                       focusPath: Option[SHACLPath],
                       sourceShape: RefNode,
                       values: Seq[RDFNode],
                       message: Seq[LiteralValue],
                       messageMap: MessageMap,
                       details: Seq[AbstractResult]
  ) extends AbstractResult {

  def setSeverity(s: Severity): ValidationResult =
    this.copy(resultSeverity = s)

  override def toString = s"Violation error on $focusNode: ${message.mkString(",")}"
}

case class MsgError(msg: String) extends AbstractResult

object ValidationResult {

 def basic(suffix: String,
           focusNode: RDFNode,
           attempt: Attempt,
           msg: String
          ) =
    ValidationResult(
      sourceConstraintComponent = sh + suffix,
      focusNode = focusNode,
      resultSeverity = attempt.severity,
      sourceShape = attempt.shapeRef,
      values = Seq(),
      focusPath = attempt.path,
      message = Seq(LiteralValue(StringLiteral(msg))),
      messageMap = attempt.messageMap,
      details = Seq()
    )

  def notFoundShapeRef(node: RDFNode, attempt: Attempt, msg: String) =
    basic("NotFoundShapeRef", node, attempt, msg)

  def expectedPropertyShape(node: RDFNode, attempt: Attempt, msg: String) =
    basic("ExpectedPropertyShape", node, attempt, msg)

  def shapesFailed(node: RDFNode, shape: Shape, ps: Set[Shape], attempt: Attempt, msg: String) =
    basic("ShapesFailed", node, attempt, msg).setSeverity(InfoSeverity)

  def regexError(node: RDFNode, attempt: Attempt, msg: String) =
    basic("RegEx error", node, attempt, msg)

  def noSiblingsError(focusNode: RDFNode, p: PropertyShape, msg: String, attempt: Attempt) =
    basic("noSiblingsError", focusNode, attempt, s"No siblings found for property shape $p in schema: $msg")

  def errorNode(node: RDFNode, shape: Shape, attempt: Attempt, msg: String): ValidationResult =
    basic("NodeConstraintComponent", node, attempt, msg)

  def classError(focusNode: RDFNode, cls: RDFNode, attempt: Attempt) =
    basic("ClassConstraintComponent", focusNode, attempt, s"Node $focusNode doesn't belong to class $cls")

  def datatypeError(focusNode: RDFNode, datatype: RDFNode, attempt: Attempt) =
    basic("DatatypeConstraintComponent", focusNode, attempt, s"Node $focusNode doesn't have dataType $datatype")

  def unsupported(focusNode: RDFNode, attempt: Attempt, msg: String) =
    basic("unsupported", focusNode, attempt, "Unsupported: " + msg)

  def notNumeric(focusNode: RDFNode, attempt: Attempt) =
    basic("NotNumericConstraintComponent", focusNode, attempt, s"NotNumeric violation. Expected $focusNode to be a number")

  def minExclusiveError(focusNode: RDFNode, attempt: Attempt, n: RDFNode) =
    basic("MinExclusiveConstraintComponent", focusNode, attempt, s"minExclusive violation. Expected $focusNode > $n")

  def minInclusiveError(focusNode: RDFNode, attempt: Attempt, n: RDFNode) =
    basic("MinInclusiveConstraintComponent", focusNode, attempt, s"minInclusive violation. Expected $focusNode >= $n")

  def maxExclusiveError(focusNode: RDFNode, attempt: Attempt, n: RDFNode) =
    basic("MaxExclusiveConstraintComponent", focusNode, attempt, s"maxExclusive violation. Expected $focusNode < $n")

  def maxInclusiveError(focusNode: RDFNode, attempt: Attempt, n: RDFNode) =
    basic("MaxInclusiveConstraintComponent", focusNode, attempt, s"maxInclusive violation. Expected $focusNode <= $n")

  def minLengthError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("MinLengthConstraintComponent", focusNode, attempt, s"minLength violation. Expected length($focusNode) >= $n")

  def maxLengthError(focusNode: RDFNode, attempt: Attempt, n: Int) =
    basic("MaxLengthConstraintComponent", focusNode, attempt, s"maxLength violation. Expected length($focusNode) <= $n")

  def patternError(focusNode: RDFNode, attempt: Attempt, p: String, flags: Option[String]) =
    basic("PatternConstraintComponent", focusNode, attempt, s"pattern violation. Expected $focusNode to match '$p'${flags.getOrElse("")}")

  def uniqueLangError(focusNode: RDFNode, attempt: Attempt, path: SHACLPath, vs: Seq[RDFNode]) =
    basic("UniqueLangConstraintComponent", focusNode, attempt, s"uniqueLang violation. Expected $focusNode to have a unique language for path $path with values $vs")

  def languageInError(focusNode: RDFNode, attempt: Attempt, langs: List[String]) =
    basic("LanguageInConstraintComponent", focusNode, attempt, s"languageIn violation. Expected $focusNode to match 'languageIn(${langs.mkString(",")})'")

  def equalsError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("EqualsConstraintComponent", focusNode, attempt, p, vs)

  def disjointError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("DisjointConstraintComponent", focusNode, attempt, p, vs)

  def lessThanError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("LessThanConstraintComponent", focusNode, attempt, p, vs)

  def lessThanOrEqualsError(focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    comparisonError("LessThanOrEqualsConstraintComponent", focusNode, attempt, p, vs)

  def comparisonError(name: String, focusNode: RDFNode, attempt: Attempt, p: IRI, vs: Set[RDFNode]) =
    basic(s"${name}ConstraintComponent", focusNode, attempt, s"$name violation. Expected $focusNode to match $name '$p', values: $vs")

  def minCountError(focusNode: RDFNode, attempt: Attempt, minCount: Int, count: Int) =
    basic("MinCountConstraintComponent", focusNode, attempt, s"MinCount violation. Expected $minCount, obtained: $count")

  def maxCountError(focusNode: RDFNode, attempt: Attempt, maxCount: Int, count: Int) =
    basic("MaxCountConstraintComponent", focusNode, attempt, s"MaxCount violation. Expected $maxCount, obtained: $count")

  def iriKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("IriConstraintComponent", focusNode, attempt, s"Node $focusNode is not an IRI")

  def literalKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("LiteralConstraintComponent", focusNode, attempt, s"Node $focusNode is not a Literal")

  def bNodeKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("BNodeConstraintComponent", focusNode, attempt, s"Node $focusNode is not a blank node")

  def bNodeOrIRIKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("BNodeOrIRIConstraintComponent", focusNode, attempt, s"Node $focusNode is not a blank node or an IRI")

  def bNodeOrLiteralKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("BNodeOrLiteralConstraintComponent", focusNode, attempt, s"Node $focusNode is not a blank node or a Literal")

  def iriOrLiteralKindError(focusNode: RDFNode, attempt: Attempt) =
    basic("IriOrLiteralConstraintComponent", focusNode, attempt, s"Node $focusNode is not a IRI or a Literal")

  def notError(focusNode: RDFNode, attempt: Attempt, shape: RefNode) =
    basic("NotConstraintComponent", focusNode, attempt, s"Not violation. Expected $focusNode not to satisfy ${shape.showId}")

  def andError(focusNode: RDFNode, attempt: Attempt, shapes: List[RefNode]) =
    basic("AndConstraintComponent", focusNode, attempt, s"And violation. Expected $focusNode to satisfy all of the shapes ${shapes.map(_.showId).mkString(",")}")

  def orError(focusNode: RDFNode, attempt: Attempt, shapes: List[RefNode]) =
    basic("OrConstraintComponent", focusNode, attempt, s"Or violation. Expected $focusNode to satisfy some of the shapes ${shapes.map(_.showId).mkString(",")}")

  def xoneError(focusNode: RDFNode, attempt: Attempt, shapes: Seq[RefNode]) =
    basic("XoneConstraintComponent", focusNode, attempt, s"Xone violation. Expected $focusNode to satisfy exactly one of the shapes ${shapes.map(_.showId).mkString(",")}")

  def qualifiedShapeError(focusNode: RDFNode, attempt: Attempt, value: Int, min: Option[Int], max: Option[Int]) =
    basic("QualifiedShapeConstraintComponent", focusNode, attempt, s"qualified shape error. Expected $focusNode to satisfy qualifiedValueShape. Value = ${value}, min: ${min.map(_.toString).getOrElse("-")}, max: ${max.map(_.toString).getOrElse("-")}")

  def hasValueError(focusNode: RDFNode, attempt: Attempt, value: Value) =
    basic("HasValueConstraintComponent", focusNode, attempt, s"HasValue error. Expected $focusNode to be  $value")

  def hasValueErrorNoValue(focusNode: RDFNode, attempt: Attempt, value: Value, path: SHACLPath) =
    basic("HasValueConstraintComponent", focusNode, attempt, s"HasValue error. Missing value for path $path on $focusNode. Value must be $value")

  def hasValueErrorMoreThanOne(focusNode: RDFNode, attempt: Attempt, value: Value, path: SHACLPath, n: Int) =
    basic("HasValueConstraintComponent", focusNode, attempt, s"HasValue error. More than one value ($n) for path $path on $focusNode. Value must be $value")

  def inError(focusNode: RDFNode, attempt: Attempt, values: Seq[Value]) =
    basic("InConstraintComponent", focusNode, attempt, s"In violation. Expected $focusNode to be in $values")

  def notShapeError(focusNode: RDFNode, shapeRef: RefNode, attempt: Attempt) =
    basic("notShape", focusNode, attempt, s"Not failed because $focusNode has shape $shapeRef and it should not have")

  def closedError(
    focusNode: RDFNode,
    attempt: Attempt,
    allowedProperties: List[IRI],
    ignoredProperties: List[IRI],
    notAllowed: List[IRI]) =
    basic("ClosedConstraintComponent", focusNode, attempt,
      s"closed violation. $focusNode has more properties than $allowedProperties. Extra properties found: $notAllowed, ignoredProperties: $ignoredProperties")

}
