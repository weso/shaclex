package es.weso.rdf.nodes

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

}
