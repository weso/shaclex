package es.weso.rdf.nodes

// It should be better to inherit from DatatypeLiteral,
// but case-to-case inheritance is prohibited in Scala
case class IntegerLiteral(int: Int, repr: String = null) extends Literal {
  val dataType = RDFNode.IntegerDatatypeIRI
  val lexicalForm = if (repr == null) int.toString else repr

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }
  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(d, r) =>
      Right (if (r != null && repr != null) r == repr else d == int)
    case _ => Right(false)
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(m, _) => Right(int < m)
    case DecimalLiteral(d, _) => Right(int < d)
    case DoubleLiteral(d, _) => Right(int < d)
    case _ => Left(s"Type error comparing $this < $other which is not numeric")
  }
}
