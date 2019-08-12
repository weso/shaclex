package es.weso.rdf.nodes

case class BooleanLiteral(bool: Boolean) extends Literal {
  val dataType = RDFNode.BooleanDatatypeIRI
  val lexicalForm = if (bool) "true" else "false"

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case BooleanLiteral(b) => Right(bool == b)
    case _ => Left(s"Type error comparing $this to non boolean literal $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case BooleanLiteral(b) => Right(bool < b)
    case _ => Left(s"Type error comparing $this < $other when other is not boolean")
  }

}

object BooleanLiteral {
  lazy val trueLiteral = BooleanLiteral(true)
  lazy val falseLiteral = BooleanLiteral(false)
}
