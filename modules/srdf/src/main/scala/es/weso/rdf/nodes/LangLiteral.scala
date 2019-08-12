package es.weso.rdf.nodes

case class LangLiteral(lexicalForm: String, lang: Lang) extends Literal {
  lazy val dataType = RDFNode.LangStringDatatypeIRI

  def isLangLiteral = true
  def hasLang(l: Lang) = lang.matchLanguage(l)

  override def toString: String = {
    val lex = "\"" + lexicalForm + "\""
    lex + lang
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case LangLiteral(l, lan) => Right(l == lexicalForm && lan == lang)
    case _ => Left(s"Type error comaring $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = other match {
    case LangLiteral(lex,lan) => Right(lexicalForm < lex && lang.lang < lan.lang)
    case _ => Left(s"Type error comaring $this with $other")
  }

}
