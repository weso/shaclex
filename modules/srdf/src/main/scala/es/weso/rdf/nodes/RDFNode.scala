package es.weso.rdf.nodes

import cats.Show

abstract class RDFNode {
  def isIRI = this match {
    case _: IRI => true
    case _ => false
  }

  def isBNode = this match {
    case _: BNodeId => true
    case _ => false
  }

  def isLiteral = this match {
    case _: Literal => true
    case _ => false
  }

  def isNonLiteral = this.isIRI || this.isBNode

  // Change this code to use Option
  def toIRI = this match {
    case i: IRI => i
    case _ =>
      throw RDFNodeException("Cannot convert RDFNode " + this + " to IRI")
  }

  def getLexicalForm: String

}

case class BNodeId(id: String) extends RDFNode {

  // @deprecated
  def newBNodeId: BNodeId = {
    val n = id.drop(1).toInt + 1
    BNodeId("b" + n)
  }

  override def toString: String = {
    "_:" + id
  }

  override def getLexicalForm = id

}

object InitialBNodeId extends BNodeId("b0")

object RDFNode {
  val xsd = "http://www.w3.org/2001/XMLSchema#"
  val rdfSyntax = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val StringDatatypeIRI = IRI(xsd + "string")
  val LangStringDatatypeIRI = IRI(rdfSyntax + "langString")
  val BooleanDatatypeIRI = IRI(xsd + "boolean")
  val IntegerDatatypeIRI = IRI(xsd + "integer")
  val DoubleDatatypeIRI = IRI(xsd + "double")
  val DecimalDatatypeIRI = IRI(xsd + "decimal")
  val rdftype = IRI(rdfSyntax + "type")
  val rdfnil = IRI(rdfSyntax + "nil")
  val rdffirst = IRI(rdfSyntax + "first")
  val rdfrest = IRI(rdfSyntax + "rest")

  val trueLiteral = BooleanLiteral(true)
  val falseLiteral = BooleanLiteral(false)

  def qNameIRI(prefix: IRI, local: String): IRI = {
    IRI(prefix.str + local)
  }

  implicit val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    final def show(n: RDFNode): String = n.toString
  }

  def fromString(s: String): Either[String, RDFNode] = {
    val iriRegex = raw"<(.*)>".r
    val bNodeRegex = raw"_:(.*)".r
    val literalRegex = "\"(.*)\"".r
    val integerRegex = raw"(\d*)".r
    s match {
      case iriRegex(iri) => Right(IRI(iri))
      case bNodeRegex(bnodeId) => Right(BNodeId(bnodeId))
      case literalRegex(str) => Right(StringLiteral(str))
      case integerRegex(s) => {
        try Right(IntegerLiteral(s.toInt))
        catch {
          case e: NumberFormatException =>
            Left(s"Error parsing as integer: $e")
        }
      }
      case _ => Left(s"Error parsing String $s as RDFNode")
    }
  }

}
