package es.weso.rdf.nodes

trait Literal extends RDFNode with Product with Serializable {
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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case DatatypeLiteral(l, d) => l == lexicalForm && d == dataType
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")

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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case IntegerLiteral(d) => d == int
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")
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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case DecimalLiteral(d) => d == decimal
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")
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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case DoubleLiteral(d) => d == double
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")
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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case StringLiteral(s) => s == lexicalForm
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")
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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case BooleanLiteral(b) => bool == b
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")

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

  def isEqualTo(other: RDFNode): Boolean = other match {
    case LangLiteral(l, lan) => l == lexicalForm && lan == lang
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")

}

