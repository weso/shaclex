package es.weso.rdf.nodes

// It should be better to inherit from DatatypeLiteral,
// but case-to-case inheritance is prohibited in Scala
case class IntegerLiteral(int: Int) extends Literal {
  val dataType = RDFNode.IntegerDatatypeIRI
  val lexicalForm = int.toString

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }
  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(d) => Right(d == int)
    case _ => Right(false)
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(m) => Right(int < m)
    case DecimalLiteral(d) => Right(int < d)
    case DoubleLiteral(d) => Right(int < d)
    case _ => Left(s"Type error comparing $this < $other which is not numeric")
  }
}
