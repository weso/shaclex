package es.weso.schema

import cats.Show
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes.{IRI, RDFNode}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import es.weso.shapeMaps._
import es.weso.rdf.jena.RDFAsJenaModel
import java.io._
import es.weso.shacl.report.ValidationReport
import es.weso.rdf.RDFBuilder
import cats.effect._


case class Result(
  isValid: Boolean,
  message: String,
  shapeMaps: Seq[ResultShapeMap],
  validationReport: RDFReport,  // Either[String,RDFReader],
  errors: Seq[ErrorInfo],
  trigger: Option[ValidationTrigger],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap,
  reportFormat: String = "TURTLE",
  ) extends LazyLogging {

  def noSolutions(shapeMaps: Seq[ResultShapeMap]): Boolean = {
    shapeMaps.size == 0 || shapeMaps.head.noSolutions
  }

  def solution: Either[String, ResultShapeMap] = {
    shapeMaps.size match {
      case 0 => Left("No solutions")
      case 1 => {
        Right(shapeMaps.head)
      }
      case _ => {
        logger.warn(s"More than one solution. Only considering the first one. Solutions:\n$shapeMaps")
        Right(shapeMaps.head)
      }
    }
  }

  def addTrigger(trigger: ValidationTrigger): Result = this.copy(trigger = Some(trigger))

  sealed abstract trait DetailsOption extends Product with Serializable
  case object Details extends DetailsOption
  case object NoDetails extends DetailsOption

  def show(base: Option[IRI], details: DetailsOption): String = {
    val sb = new StringBuilder
    if (isValid) {
      if (shapeMaps.size == 0) {
        sb ++= s"No solutions"
      } else {
        for ((solution, n) <- shapeMaps zip (1 to cut)) {
          sb ++= "Result " + printNumber(n, cut)
          sb ++= solution.relativize(base).toString
        }
      }
    } else
      sb ++= errors.map(_.show).mkString("\n")
    sb.toString
  }

  def toJson(builder: RDFBuilder): IO[Json] = {

    implicit val encodePrefixMap: Encoder[PrefixMap] = new Encoder[PrefixMap] {
      final def apply(prefixMap: PrefixMap): Json = {
        val newMap: Map[String, Json] = prefixMap.pm.toList.map {
          case (prefix, iri) => (prefix.str, Json.fromString(iri.getLexicalForm))
        }.toMap
        Json.fromJsonObject(JsonObject.fromMap(newMap))
      }
    }

    def result2Json(result: Result, strValidationReport: String): Json = Json.obj(
          ("valid", isValid.asJson),
          ("type", "Result".asJson),
          ("message", message.asJson),
          ("shapeMap", solution match {
            case Left(msg) => Json.fromString(msg)
            case Right(r) => r.toJson
          }),
          ("errors", errors.toList.asJson),
          ("nodesPrefixMap", nodesPrefixMap.asJson),
          ("shapesPrefixMap", shapesPrefixMap.asJson),
          ("validationReport", Json.fromString(strValidationReport))
    )

    for {
      rdf <- validationReport.toRDF(builder)
      str <- rdf.serialize(reportFormat)
    } yield {
      result2Json(this, str)
    }
  } 

  private def toJsonString2spaces(builder: RDFBuilder): IO[String] =
    toJson(builder)map(_.spaces2)

  private lazy val cut = 1 // TODO maybe remove concept of cut

  private def printNumber(n: Int, cut: Int): String = {
    if (n == 1 && cut == 1) ""
    else n.toString
  }

  def serialize(format: String, base: Option[IRI] = None, builder: RDFBuilder): IO[String] = format.toUpperCase match {
    case Result.COMPACT => IO(show(base, NoDetails))
    case Result.DETAILS => IO(show(base, Details))
    case Result.JSON => toJsonString2spaces(builder)
    case _ => IO.raiseError(new RuntimeException(s"Unsupported format to serialize result: $format, $this"))
  }

  def hasShapes(node: RDFNode): Seq[SchemaLabel] = {
    solution match {
      case Left(str) => Seq()
      case Right(s) => s.hasShapes(node).map(cnvLabel)
    }
  }

  def cnvLabel(lbl: ShapeMapLabel): SchemaLabel = {
    lbl match {
      case IRILabel(iri) => SchemaLabel(iri, shapesPrefixMap)
      case BNodeLabel(bn) => SchemaLabel(bn, shapesPrefixMap)
      case Start => throw new Exception("Result.cnvLabel: Unexpected conversion from Start to SchemaLabel")
    }
  }
}

object Result extends LazyLogging {
  def empty =
    Result(
      isValid = true,
      message = "",
      shapeMaps = Seq(),
      validationReport = RDFReport.empty,
      errors = Seq(),
      None,
      PrefixMap.empty,
      PrefixMap.empty)

  def errStr(str: String) =
    empty.copy(isValid = false, message = str)

  implicit val showResult = new Show[Result] {
    override def show(r: Result): String = r.show(None)
  }

  lazy val TEXT = "COMPACT"
  lazy val JSON = "JSON"
  lazy val DETAILS = "DETAILS"

  lazy val availableResultFormats = List(TEXT, JSON, DETAILS).map(_.toUpperCase)
  lazy val defaultResultFormat = availableResultFormats.head

  // TODO: implement this
  implicit val decodeTrigger: Decoder[Option[ValidationTrigger]] = Decoder.instance { c =>
    {
      logger.warn("Trigger decoder not implemented")
      ??? //Right(None)
    }
  }

  // TODO: implement this
  implicit val decodePrefixMap: Decoder[PrefixMap] = Decoder.instance { c =>
    {
      logger.warn("PrefixMap decoder not implemented")
      ??? //Right(None)
    }
  }

  // TODO: implement this
  implicit val decodeResultShapeMap: Decoder[ResultShapeMap] = Decoder.instance { c =>
    {
      throw new Exception("unimplemented decodeResulShapeMap")
    }
  }

  implicit val decodeResult: Decoder[Result] = Decoder.instance { c =>
    for {
      isValid <- c.get[Boolean]("valid")
      message <- c.get[String]("message")
      nodesPrefixMap <- c.get[PrefixMap]("nodesPrefixMap")
      shapesPrefixMap <- c.get[PrefixMap]("shapesPrefixMap")
      trigger <- c.get[Option[ValidationTrigger]]("trigger")
      solutions <- if (isValid) {
        for {
          ls <- c.downField("shapeMap").as[List[ResultShapeMap]]
        } yield ls
      } else Right(Seq())
      errors <- if (isValid) {
        Right(Seq())
      } else for {
        ls <- c.downField("shapeMap").as[List[ErrorInfo]]
      } yield ls
    } yield Result(
      isValid,
      message,
      solutions,
      RDFReport.empty,
      errors,
      trigger,
      nodesPrefixMap,
      shapesPrefixMap)
  }

}
