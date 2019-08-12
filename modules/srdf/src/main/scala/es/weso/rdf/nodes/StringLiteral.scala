package es.weso.rdf.nodes

case class StringLiteral(lexicalForm: String) extends Literal {
  override val dataType = RDFNode.StringDatatypeIRI

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    // TODO: Check if literal contains extended chars
    "\"" + lexicalForm + "\""
  }

  override def getLexicalForm = lexicalForm

  override def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case StringLiteral(s) => Right(s == lexicalForm)
    case _ => Right(false)
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case StringLiteral(str) => Right(lexicalForm < str)
    case _ => Left(s"Cannot compare string literal $this < $other which is not a string")
  }
}
