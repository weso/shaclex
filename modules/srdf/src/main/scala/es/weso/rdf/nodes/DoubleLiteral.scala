package es.weso.rdf.nodes

case class DoubleLiteral(double: Double, repr: String = null) extends Literal {
  val dataType = RDFNode.DoubleDatatypeIRI
  val lexicalForm = if (repr == null) double.toString else repr

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n, _) => Right(n == double)
    case DoubleLiteral(d, r) => Right(if (r!=null && repr != null) r == repr else d == double)
    case DecimalLiteral(d, _) => Right(d == double)
    case _ => Left(s"Type error comparing $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case IntegerLiteral(n, _) => Right(double < n)
    case DecimalLiteral(d, _) => Right(double < d)
    case DoubleLiteral(d, _) => Right(double < d)
    case _ => Left(s"Type error comparing $this with $other")
  }
}
