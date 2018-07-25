package es.weso.rdf.nodes

case class DecimalLiteral(decimal: BigDecimal) extends Literal {
  val dataType = RDFNode.DecimalDatatypeIRI
  val lexicalForm = decimal.toString

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }
  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n) => Right(n == decimal)
    case DoubleLiteral(d) => Right(d == decimal)
    case DecimalLiteral(d) => Right(d == decimal)
    case _ => Left(s"Type error comparing $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n) => Right(decimal < n)
    case DecimalLiteral(d) => Right(decimal < d)
    case DoubleLiteral(d) => Right(decimal < d)
    case _ => Left(s"Type error comparing $this < $other which is non numeric")
  }
}
