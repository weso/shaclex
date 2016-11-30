package es.weso.schema
import Explanation._
import es.weso.rdf.PrefixMap
import cats._
import data._
import implicits._
import es.weso.shex.implicits.showShEx
import io.circe._
import io.circe.JsonObject._
import io.circe.syntax._

case class InfoNode(
    hasShapes: Seq[(SchemaLabel,Explanation)],
    hasNoShapes: Seq[(SchemaLabel,Explanation)],
    pm: PrefixMap
    ) {


  def contains(label: SchemaLabel): Boolean = {
    hasShapes.map(_._1).contains(label)
  }

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

  def conditionalAdd(cond: Boolean, obj: JsonObject, key: String, value: Json): JsonObject = {
    if (cond) obj.add(key,value)
    else obj
  }

  def toJson: Json = {
    val jsonPositive: Json = Json.fromJsonObject(
      JsonObject.from(hasShapes.toList.map{case (label,e) => (label.show, Json.fromString(e.str))})
    )
    val jsonNegative: Json = Json.fromJsonObject(
      JsonObject.from(hasNoShapes.toList.map{case (label,e) => (label.show, Json.fromString(e.str))})
    )
    Json.fromJsonObject{
     val obj = JsonObject.empty
     val pos = conditionalAdd(!hasShapes.isEmpty, obj, "hasShapes", jsonPositive)
     val neg = conditionalAdd(!hasNoShapes.isEmpty, pos, "hasNoShapes", jsonNegative)
     neg
    }
  }
}

object InfoNode {

  implicit val showInfoNode = new Show[InfoNode] {
    override def show(n: InfoNode): String = {
      n.show
    }
  }

  implicit val encodeInfoNode: Encoder[InfoNode] = new Encoder[InfoNode] {
    final def apply(i: InfoNode): Json = i.toJson
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
