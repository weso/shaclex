package es.weso.schema
import cats.Show
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode
import io.circe.JsonObject._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.either._
import cats.instances.either._

case class Result(
    isValid: Boolean,
    message: String,
    solutions: Seq[Solution],
    errors: Seq[ErrorInfo],
    trigger: Option[ValidationTrigger]) extends LazyLogging {

  def noSolutions(sols: Seq[Solution]): Boolean = {
    sols.size == 0 || sols.head.nodes.isEmpty
  }

  def solution: Either[String,Solution] = {
    solutions.size match {
      case 0 => Left("No solutions")
      case 1 => Right(solutions.head)
      case _ => {
        logger.warn(s"More than one solution. Only consider the first one. Solutions:\n$solutions")
        Right(solutions.head)
      }
    }
  }

  def addTrigger(trigger: ValidationTrigger): Result = this.copy(trigger = Some(trigger))

  def show: String = {
    val sb = new StringBuilder
    if (isValid) {
      if (solutions.size == 0) {
        sb ++= s"No solutions"
      } else {
          for ((solution, n) <- solutions zip (1 to cut)) {
            sb ++= "Result " + printNumber(n, cut)
            sb ++= solution.show
          }
        }
    }
    else
      sb ++= errors.map(_.show).mkString("\n")
    sb.toString
  }

  def toJson: Json = {

    implicit val encodeResult: Encoder[Result] = new Encoder[Result] {
      final def apply(a: Result): Json = {
        val details: Json = if (isValid) {
          solutions.toList.asJson
        } else {
          errors.toList.asJson
        }
        Json.fromJsonObject(JsonObject.empty.
          add("valid",Json.fromBoolean(isValid)).
          add("type",Json.fromString("Result")).
          add("message",Json.fromString(message)).
          add("details",details).
          add("trigger",trigger.asJson)
        )
      }
    }
    this.asJson
  }

  def toJsonString2spaces: String =
    toJson.spaces2

  lazy val cut = 1 // TODO maybe remove concept of cut

  def printNumber(n: Int, cut: Int): String = {
    if (n == 1 && cut == 1) ""
    else n.toString
  }

  def serialize(format: String): String = format.toUpperCase match {
    case Result.TEXT => show
    case Result.JSON => toJsonString2spaces
    case _ => s"Unsupported format to serialize result: $format, $this"
  }

  def hasShapes(node: RDFNode): Seq[SchemaLabel] = {
    solution match {
      case Left(str) => Seq()
      case Right(s) => s.hasShapes(node)
    }
  }

}

object Result extends LazyLogging {
  def empty =
    Result(isValid = true,
      message = "",
      solutions = Seq(),
      errors=Seq(),
      None
    )

  def errStr(str: String) = Result(isValid = false, message = str, solutions = Seq(), errors = Seq(), None)

  implicit val showResult = new Show[Result] {
    override def show(r: Result): String = r.show
  }

  lazy val TEXT = "TEXT"
  lazy val JSON = "JSON"
  lazy val availableResultFormats = List(TEXT, JSON).map(_.toUpperCase)
  lazy val defaultResultFormat = availableResultFormats.head

  // TODO: take into account trigger
  implicit val decodeResult: Decoder[Result] = Decoder.instance { c =>
    logger.info(s"Decoding result: $c")
    for {
      isValid <- c.get[Boolean]("valid")
      message <- c.get[String]("message")
      solutions <- if (isValid) {
        for {
          ls <- c.downField("details").as[List[Solution]]
        } yield ls.toSeq
      } else Right(Seq())
      errors <- if (isValid) {
        Right(Seq())
      } else for {
        ls <- c.downField("details").as[List[ErrorInfo]]
      } yield ls.toSeq
    } yield Result(isValid,message,solutions,errors,None)
  }


}
