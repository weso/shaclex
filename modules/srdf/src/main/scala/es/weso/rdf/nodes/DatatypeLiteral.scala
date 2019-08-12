package es.weso.rdf.nodes
import es.weso.rdf.PREFIXES._
import es.weso.utils.XMLUtils._

case class DatatypeLiteral(lexicalForm: String, dataType: IRI) extends Literal {
  override def isLangLiteral = false
  override def hasLang(lang: Lang) = false
  override def toString: String = {
    "\"" + lexicalForm + "\"^^" + dataType
  }

  override def getLexicalForm = lexicalForm

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case DatatypeLiteral(l, d) => Right(l == lexicalForm && d == dataType)
    case _ => Left(s"Type error comaring $this with $other")
  }

  override def lessThan(other: RDFNode): Either[String,Boolean] = this.dataType match {
    case `xsd:dateTime` => other match {
      case DatatypeLiteral(otherDate, `xsd:dateTime`) => lessThanXSDDateTimes(lexicalForm,otherDate)
      case _ => Left(s"Type error comaring $this with $other which is not xsd:dateTime")
    }
    case _ => Right(lexicalForm < other.getLexicalForm)
  }

}
