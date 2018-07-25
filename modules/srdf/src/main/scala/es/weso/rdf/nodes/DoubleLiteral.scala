package es.weso.rdf.nodes

case class DoubleLiteral(double: Double) extends Literal {
  val dataType = RDFNode.DoubleDatatypeIRI
  val lexicalForm = double.toString

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n) => Right(n == double)
    case DoubleLiteral(d) => Right(d == double)
    case DecimalLiteral(d) => Right(d == double)
    case _ => Left(s"Type error comparing $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n) => Right(double < n)
    case DecimalLiteral(d) => Right(double < d)
    case DoubleLiteral(d) => Right(double < d)
    case _ => Left(s"Type error comparing $this with $other")
  }
}
