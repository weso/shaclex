package es.weso.shacl
import cats._
import cats.implicits._
import es.weso.rdf.nodes.{Lang, LangLiteral, RDFNode, StringLiteral}

case class MessageMap(mmap: Map[Option[Lang], String]) {

  def getRDFNodes: List[RDFNode] = mmap.toList.map {
    case (maybeLang, str) =>
      maybeLang match {
        case None       => StringLiteral(str)
        case Some(lang) => LangLiteral(str, lang)
      }
  }

  def addMessage(node: RDFNode): Either[String, MessageMap] = node match {
    case StringLiteral(str) => mmap.get(None) match {
      case None => Right(MessageMap(mmap.updated(None, str)))
      case Some(other) => Left(s"Trying to create two messages without language tag: $other and $str")
    }
    case LangLiteral(str,lang) => mmap.get(Some(lang)) match {
      case None => Right(MessageMap(mmap.updated(Some(lang), str)))
      case Some(other) => Left(s"Trying to create two messages with same language tag ($lang): $other and $str")
    }
    case _ => Left(s"Node $node must be a string or a language tagged string to be a message")
  }

  override def toString(): String = Show[MessageMap].show(this)

}

object MessageMap {

  def fromString(msg: String): MessageMap = {
    MessageMap(Map(None -> msg))
  }

  def fromRDFNodes(nodes: List[RDFNode]): Either[String, MessageMap] = {
    val zero: Either[String,MessageMap] = Right(MessageMap(Map()))
    def cmb(rest: Either[String,MessageMap], x: RDFNode): Either[String,MessageMap] = for {
      mmap <- rest
      r <- mmap.addMessage(x)
      } yield r
    nodes.foldLeft(zero)(cmb)
  }

  implicit def monoidMessageMap: Monoid[MessageMap] = new Monoid[MessageMap] {
      override def empty: MessageMap = MessageMap(Map())

      override def combine(m1: MessageMap, m2: MessageMap): MessageMap =
        MessageMap(m1.mmap |+| m2.mmap)
    }

  implicit def showMessageMap: Show[MessageMap] = new Show[MessageMap] {
    override def show(m: MessageMap): String = {
      m.mmap.toList.map { case (maybeLang,msg) =>
        maybeLang match {
          case None => msg
          case Some(lang) => s"$msg@$lang"
        }
      }.mkString(",")
    }
  }

  def empty: MessageMap = Monoid[MessageMap].empty

}