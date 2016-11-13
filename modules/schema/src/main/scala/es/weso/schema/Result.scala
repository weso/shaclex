package es.weso.schema
import cats.Show
import es.weso.rdf.PrefixMap
import io.circe.JsonObject._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

case class Result(
    isValid: Boolean,
    message: String,
    solutions: Seq[Solution],
    errors: Seq[ErrorInfo]) {

  def noSolutions(sols: Seq[Solution]): Boolean = {
    sols.size == 1 && sols.head.isEmpty
  }

  def show: String = {
    val sb = new StringBuilder
    if (isValid) {
      if (noSolutions(solutions)) {
        "No solutions found"
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
    implicit val encodeSolution: Encoder[Solution] = new Encoder[Solution] {
      final def apply(a: Solution): Json = Json.fromJsonObject(
        singleton("type",Json.fromString("Solution"))
      )
    }

    implicit val encodeErrorInfo: Encoder[ErrorInfo] = new Encoder[ErrorInfo] {
      final def apply(a: ErrorInfo): Json = Json.fromJsonObject(
        singleton("type",Json.fromString("ErrorInfo"))
      )
    }

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
          add("details",details))
      }
    }
    this.asJson
  }

  def toJsonString2spaces: String =
    toJson.spaces2


  /*  def toHTML(cut: Int = 1, schema:Schema): String = {
    val sb = new StringBuilder
    val pm = schema.pm
    if (isValid) {
      if (noSolutions(solutions)) {
        sb ++= "<h2>No solutions found</h2"
      } else {
     for ((solution, n) <- solutions zip (1 to cut)) {
      sb ++= "<h2 class='result'>Result" + printNumber(n, cut) + "</h2>"
      sb ++= schema.htmlBeforeSolutions
      sb ++= solution.toHTML(pm)
      sb ++= schema.htmlAfterSolutions
     }
     }
    } else {
    val numErrors = errors.size
    val errorStr = if (numErrors == 1) "Error" else "Errors"
    sb ++="<div class=\"errors\">"
    sb ++= s"<p class='errorMsg'>${numErrors} $errorStr found</p>"
    sb ++= "<table class='display' id='results' >"
    sb ++= schema.htmlBeforeErrors
    for (error <- errors) {
      sb ++= error.toHTML(pm)
     }
    sb ++= schema.htmlAfterErrors
    sb++="</table>"
    }
    sb.toString
  }

  }
 */

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

}

object Result {
  def empty =
    Result(isValid = true,
           message = "",
           solutions = Seq(),
           errors=Seq())

  def errStr(str: String) = Result(isValid = false, message = str, solutions = Seq(), errors = Seq())

  implicit val showResult = new Show[Result] {
    override def show(r: Result): String = r.show
  }

  lazy val TEXT = "TEXT"
  lazy val JSON = "JSON"
  lazy val availableResultFormats = List(TEXT, JSON).map(_.toUpperCase)
  lazy val defaultResultFormat = availableResultFormats.head

}
