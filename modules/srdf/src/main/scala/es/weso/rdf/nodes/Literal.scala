package es.weso.rdf.nodes

trait Literal extends RDFNode {
  def dataType: IRI

  def isLangLiteral: Boolean
  def hasLang(lang: Lang): Boolean
  def hasDatatype(iri: IRI): Boolean =
    dataType == iri
  override def getLexicalForm: String
}

case class DatatypeLiteral(lexicalForm: String, dataType: IRI) extends Literal {
  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false
  override def toString: String = {
    "\"" + lexicalForm + "\"^^" + dataType
  }

  override def getLexicalForm = lexicalForm
}

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

}

case class DecimalLiteral(decimal: BigDecimal) extends Literal {
  val dataType = RDFNode.DecimalDatatypeIRI
  val lexicalForm = decimal.toString

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }
  override def getLexicalForm = lexicalForm

}

case class DoubleLiteral(double: Double) extends Literal {
  val dataType = RDFNode.DoubleDatatypeIRI
  val lexicalForm = double.toString

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm
}

case class StringLiteral(lexicalForm: String) extends Literal {
  val dataType = RDFNode.StringDatatypeIRI

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    // TODO: Check if literal contains extended chars
    "\"" + lexicalForm + "\""
  }

  override def getLexicalForm = lexicalForm
}

case class BooleanLiteral(bool: Boolean) extends Literal {
  val dataType = RDFNode.BooleanDatatypeIRI
  val lexicalForm = if (bool) "true" else "false"

  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false

  override def toString: String = {
    lexicalForm
  }

  override def getLexicalForm = lexicalForm
}

object BooleanLiteral {
  lazy val trueLiteral = BooleanLiteral(true)
  lazy val falseLiteral = BooleanLiteral(false)
}

case class LangLiteral(lexicalForm: String, lang: Lang) extends Literal {
  lazy val dataType = RDFNode.LangStringDatatypeIRI

  def isLangLiteral = true
  def hasLang(l: Lang) = lang.matchLanguage(l)

  override def toString: String = {
    val lex = "\"" + lexicalForm + "\""
    lex + lang
  }

  override def getLexicalForm = lexicalForm
}

