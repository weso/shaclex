package es.weso.rdf.nodes

case class DecimalLiteral(decimal: BigDecimal, repr: String = null) extends Literal {
  val dataType = RDFNode.DecimalDatatypeIRI
  val lexicalForm = if (repr == null) decimal.toString else repr

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n, _) => Right(n == decimal)
    case DoubleLiteral(d, r)  => Right(if (r != null && repr != null) r == repr else d == decimal)
    case DecimalLiteral(d, _) => Right(d == decimal)
    case _ => Left(s"Type error comparing $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n, _) => Right(decimal < n)
    case DecimalLiteral(d, _) => Right(decimal < d)
    case DoubleLiteral(d, _) => Right(decimal < d)
    case _ => Left(s"Type error comparing $this < $other which is non numeric")
  }
}
