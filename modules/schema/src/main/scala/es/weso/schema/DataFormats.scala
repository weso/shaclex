package es.weso.schema

import scala.util._

case class DataFormats(name: String)

object DataFormats {
  lazy val TURTLE = DataFormats("TURTLE")
  lazy val RDFXML = DataFormats("RDF/XML")
  lazy val JSONLD = DataFormats("JSON-LD")
  lazy val NTRIPLES = DataFormats("N-TRIPLES")
  lazy val RDFJSON = DataFormats("RDF/JSON")
  lazy val TRIG = DataFormats("TRIG")
  lazy val DOT = DataFormats("DOT")

  lazy val availableFormats: Seq[DataFormats] =
    List(TURTLE, RDFXML, JSONLD, NTRIPLES, RDFJSON, TRIG, DOT)

  lazy val formatNames: Seq[String] =
    availableFormats.map(_.name)

  def available(format: String): Boolean = {
    formatNames.contains(format.toUpperCase)
  }

  def default = TURTLE
  def defaultFormatName = TURTLE.name

  lazy val toList: List[String] = availableFormats.map(_.name).toList

  override def toString(): String = {
    toList.mkString(",")
  }

  def lookup(format: String): Try[DataFormats] = {
    availableFormats.find(_.name == format.toUpperCase).headOption match {
      case Some(df) => Success(df)
      case None => Failure(new Exception(s"Not found format $format in ${availableFormats.toList}"))
    }
  }
}