package es.weso.schema
import cats._, data._
import implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap

case class Solution
  (map: Map[RDFNode,InfoNode],
   nodeMap: PrefixMap,
   schemaMap: PrefixMap
  ) {

  override def toString: String = show

  def show: String = {
    val sb = new StringBuilder
    sb ++= "Solution\n"
    for (pair <- map.toSeq) {
      val (node,info) = pair
      sb ++= ( nodeMap.qualify(node) + " " + info.show + "\n" )
    }
    sb.toString
  }

/*  def toHTML(pm: PrefixMap): String = {
    val sb = new StringBuilder
    sb ++= "<h2>Solution</h2>"
    sb ++= """<table class=\"result\"><tr><th>Node</th><th>Shapes</th></tr>"""
    for (pair <- map.toSeq) {
      val (node,info) = pair
      sb ++= ("<tr><td class=\"node\">" + node2Html(node,pm) + "</td>" +
              "<td class=\"shapes\">" + info.toHTML(pm) + "</td></tr>")
    }
    sb ++= "</table>"
    sb.toString
  }

   def node2Html(node: RDFNode, pm:PrefixMap): String = {
    if (node.isIRI) code(showIRI(node.toIRI)(pm))
    else code(node.toString)
  }

  def code(str: String): String = {
    s"<code>${escape(str)}</code>"
  }
 */
  def isEmpty : Boolean = {
    map.isEmpty
  }

}

object Solution {
  implicit val showSolution = new Show[Solution] {
    override def show(s: Solution): String = {
     s.show
   }
  }
}
