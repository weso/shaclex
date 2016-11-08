package es.weso.schema
import Explanation._
import es.weso.rdf.PrefixMap
import cats._, data._
import implicits._
import es.weso.shex.implicits.showShEx

case class InfoNode(
    hasShapes: Seq[(SchemaLabel,Explanation)],
    hasNoShapes: Seq[(SchemaLabel,Explanation)],
    pm: PrefixMap
    ) {
  override def toString: String = show

  def show: String = {
    val sb = new StringBuilder
    for ((s,e) <- hasShapes) {
      sb ++= ("+" + s.show + " " + e.str)
    }
    for ((s,e) <- hasNoShapes) {
      sb ++= ("-" + s.show + " " + e.str)
    }
    sb.toString
  }
}

object InfoNode {

  implicit val showInfoNode = new Show[InfoNode] {
    override def show(n: InfoNode): String = {
      n.show
    }
  }


/*  def toHTML(pm: PrefixMap): String = {
    val sb = new StringBuilder
    sb ++= "<ul class=\"positiveShapes\">"
    for ((s,e) <- hasShapes) {
      sb ++= ("<li><span class=\"shape\"" + s.toHTML(pm) + "</span>" +
              "<span class=\"explanation\">" + e.toHTML(pm) + "</span></li>")
    }
    sb.append("</ul>")
    sb.append("<ul class=\"negativeShapes\">")
    for ((s,e) <- hasNoShapes) {
      sb ++= ("<li><span class=\"shape\"" + s.toHTML(pm) + "</span>" +
              "<span class=\"explanation\">" + e.toHTML(pm) + "</span></li>")
    }
    sb.append("</ul>")
    sb.toString
  } */

}
