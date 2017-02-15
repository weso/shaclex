package es.weso.rdf.nodes

trait Literal extends RDFNode {
  def dataType: IRI

  def isLangLiteral: Boolean
  def hasLang(lang: Lang): Boolean
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

case class Lang(lang: String) {

  // This should be the right regular expression for lang.
  // We don't use this expression because the specification does not also.
  val langtag_ex: String = "(\\A[xX]([\\x2d]\\p{Alnum}{1,8})*\\z)" +
    "|(((\\A\\p{Alpha}{2,8}(?=\\x2d|\\z)){1}" +
    "(([\\x2d]\\p{Alpha}{3})(?=\\x2d|\\z)){0,3}" +
    "([\\x2d]\\p{Alpha}{4}(?=\\x2d|\\z))?" +
    "([\\x2d](\\p{Alpha}{2}|\\d{3})(?=\\x2d|\\z))?" +
    "([\\x2d](\\d\\p{Alnum}{3}|\\p{Alnum}{5,8})(?=\\x2d|\\z))*)" +
    "(([\\x2d]([a-wyzA-WYZ](?=\\x2d))([\\x2d](\\p{Alnum}{2,8})+)*))*" +
    "([\\x2d][xX]([\\x2d]\\p{Alnum}{1,8})*)?)\\z"

  // TODO. Specification defines other ways to match languages
  def matchLanguage(other: Lang) =
    this.lang.toLowerCase == other.lang.toLowerCase

  override def toString = lang match {
    case "" => ""
    case ls => "@" + ls
  }

  // The following code has been inspired by:
  // http://stackoverflow.com/questions/7681183/how-can-i-define-a-custom-equality-operation-that-will-be-used-by-immutable-set
  override def equals(o: Any) = o match {
    case that: Lang => that.lang.toLowerCase == this.lang.toLowerCase
    case _ => false
  }

  override def hashCode = lang.toLowerCase.hashCode
}
