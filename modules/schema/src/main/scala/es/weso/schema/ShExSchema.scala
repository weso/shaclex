package es.weso.schema

// import cats._
// import cats.data._
import cats.effect._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shapemaps._
import es.weso.shex.converter.ShEx2Shacl
import es.weso.shex.shexR._
import es.weso.shex.validator.{Result => _, _}
import es.weso.shex.{ResolvedSchema, Schema => SchemaShEx}
import es.weso.slang.{SLang2Clingo, ShEx2SLang}
import es.weso.utils.VerboseLevel

import scala.util._
import scala.util.control.NoStackTrace

case class ShExSchema(schema: SchemaShEx)
  extends es.weso.schema.Schema
    with LazyLogging
    with SLang2Clingo
    with ShEx2SLang {
  override def name = "ShEx"

  lazy val shExCFormat = "ShExC"
  lazy val shExJFormat = "ShExJ"
  //  lazy val svgFormat = "SVG"

  def getValidator(builder: RDFBuilder): IO[Validator] =
    for {
      resolvedSchema <- ResolvedSchema.resolve(schema, None, VerboseLevel.Nothing, None)
    } yield Validator(schema = resolvedSchema, builder = builder)

  // TODO: Separate input/output formats
  override def formats: List[String] =
    List(shExCFormat, shExJFormat) ++
      RDFAsJenaModel.availableFormats
  // ++ List(svgFormat)

  lazy val formatsUpperCase: List[String] = formats.map(_.toUpperCase)

  override def defaultTriggerMode: ValidationTrigger = ShapeMapTrigger.empty

  override def validate(rdf: RDFReader, trigger: ValidationTrigger, builder: RDFBuilder): IO[Result] =
    (trigger match {
      case TargetDeclarations => validateTargetDecls(rdf, builder)
      //    case MapTrigger(sm, ns) => validateShapeMap(sm, ns, rdf)
      case ShapeMapTrigger(sm) =>
        for {
          pm             <- rdf.getPrefixMap
          eitherFixedMap <- ShapeMap.fixShapeMap(sm, rdf, pm, schema.prefixes.getOrElse(PrefixMap.empty)).attempt
          result <- eitherFixedMap match {
            case Left(e)              => IO(Result.errStr(s"Error fixing shape map: $e"))
            case Right(fixedShapeMap) => validateFixedShapeMap(fixedShapeMap, rdf, builder)
          }
        } yield result
    }).map(_.addTrigger(trigger))

  def validateTargetDecls(rdf: RDFReader, builder: RDFBuilder): IO[Result] =
    for {
      validator <- getValidator(builder)
      r         <- validator.validateNodeDecls(rdf, VerboseLevel.Nothing)
      pm        <- rdf.getPrefixMap
      eitherResultShapeMap = r.toEitherS.map(_._2)
      converted            = cnvResult(eitherResultShapeMap, rdf, pm)
    } yield converted

  def cnvResult(r: Either[String, ResultShapeMap], rdf: RDFReader, pm: PrefixMap): Result = r match {
    case Left(msg) =>
      Result(
        isValid = false,
        message = "Error validating",
        shapeMaps = Seq(),
        validationReport = EmptyReport,
        errors = Seq(ErrorInfo(msg)),
        None,
        pm,
        schema.prefixMap
      )
    case Right(resultShapeMap) =>
      val allValid = resultShapeMap.isAllConformant
      Result(
        isValid = allValid,
        if (allValid) "Validated" else "Validated with errors",
        shapeMaps = Seq(resultShapeMap),
        validationReport = EmptyReport,
        errors = resultShapeMap.getAllErrors.map(ErrorInfo(_)),
        None,
        pm,
        schema.prefixMap
      )
  }

  def validateFixedShapeMap(fixedShapeMap: FixedShapeMap, rdf: RDFReader, builder: RDFBuilder): IO[Result] = {
    validateShapeMap(fixedShapeMap, rdf, builder)
  }

  def validateNodeShape(node: IRI, shape: String, rdf: RDFReader, builder: RDFBuilder): IO[Result] =
    for {
      validator <- getValidator(builder)
      r         <- validator.validateNodeShape(rdf, node, shape, VerboseLevel.Nothing)
      pm        <- rdf.getPrefixMap
      eitherResultShapeMap = r.toEitherS.map(_._2)
      res                  = cnvResult(eitherResultShapeMap, rdf, pm)
    } yield res

  def validateNodeStart(node: IRI, rdf: RDFReader, builder: RDFBuilder): IO[Result] =
    for {
      validator <- getValidator(builder)
      r         <- validator.validateNodeStart(rdf, node, VerboseLevel.Nothing)
      pm        <- rdf.getPrefixMap
      eitherResultShapeMap = r.toEitherS.map(_._2)
      res                  = cnvResult(eitherResultShapeMap, rdf, pm)
    } yield res

  def validateShapeMap(shapeMap: FixedShapeMap, rdf: RDFReader, builder: RDFBuilder): IO[Result] =
    for {
      validator <- getValidator(builder)
      r         <- validator.validateShapeMap(rdf, shapeMap, VerboseLevel.Nothing).attempt
      pm        <- rdf.getPrefixMap
      res <- r match {
        case Left(error) =>
          IO(
            Result(
              isValid = false,
              "Error validating",
              Seq(),
              EmptyReport,
              Seq(ErrorInfo(error.getMessage)),
              None,
              pm,
              schema.prefixMap
            )
          )
        case Right(result) =>
          for {
            resultShapeMap <- result.toResultShapeMap
          } yield {
            val allValid = resultShapeMap.isAllConformant
            Result(
              isValid = allValid,
              message = if (allValid) "Validated" else "Validated with errors",
              shapeMaps = Seq(resultShapeMap),
              validationReport = EmptyReport,
              errors = resultShapeMap.getAllErrors.map(ErrorInfo(_)),
              trigger = None,
              nodesPrefixMap = pm,
              shapesPrefixMap = schema.prefixMap
            )
          }
      }
    } yield res

  def cnvViolationError(v: ShExError): ErrorInfo = {
    ErrorInfo(v.show)
  }

  override def fromString(str: String, format: String, base: Option[String]): IO[Schema] = {
    ShExSchema.fromString(str, format, base)
  }

  private def handleErr[A](e: Either[String, A]): IO[A] = e.fold(
    s => err(s),
    IO.pure
  )

  override def fromRDF(rdf: RDFReader): IO[es.weso.schema.Schema] =
    for {
      eitherSchema <- RDF2ShEx.rdf2Schema(rdf)
      schema <- IO.fromEither(
        eitherSchema.leftMap(s => ShExSchemaError(s"Error obtaining schema from RDF: $s\nRDF:\n${rdf.rdfReaderName}"))
      )
    } yield ShExSchema(schema)

  override def serialize(format: String, base: Option[IRI]): IO[String] = {
    val fmt = format.toUpperCase
    RDFAsJenaModel.empty.flatMap(
      _.use(builder =>
        for {
          str <- fmt.toUpperCase match {
            case _ if formatsUpperCase.contains(fmt) => SchemaShEx.serialize(schema, fmt, base, builder)
            case _                                   => err(s"Can't serialize to format $format. Supported formats=$formats")
          }
        } yield str
      )
    )
  }

  case class ShExSchemaError(msg: String) extends RuntimeException(msg) with NoStackTrace
  private def err[A](msg: String): IO[A] = IO.raiseError(ShExSchemaError(msg))

  override def empty: es.weso.schema.Schema = ShExSchema(schema = SchemaShEx.empty)

  override def shapes: List[String] = {
    val pm = schema.prefixMap
    schema.labels.map(lbl => pm.qualify(lbl.toRDFNode))
  }

  override def pm: PrefixMap = schema.prefixMap

  override def convert(
                        targetFormat: Option[String],
                        targetEngine: Option[String],
                        base: Option[IRI]
                      ): IO[String] = {
    targetEngine.map(_.toUpperCase) match {
      case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base)
      case Some(engine) if (engine.equalsIgnoreCase(name)) => {
        serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base)
      }
      case Some("SHACL") | Some("SHACLEX") =>
        RDFAsJenaModel.empty.flatMap(
          _.use(builder =>
            for {
              newSchema <- handleErr(ShEx2Shacl.shex2Shacl(schema, None).leftMap(es => es.mkString("\n")))
              // TODO: Check if we should pass base
              str <- newSchema.serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base, builder)
            } yield str
          )
        )
      case Some(unknown) => mkErr(s"Conversion from ShEx to $unknown not implemented yet")
    }
  }

  private def mkErr[A](s: String): IO[A] = IO.raiseError(new RuntimeException(s))

  override def info: SchemaInfo = {

    val reasons: List[String] = schema.negCycles.fold(
      e => List(e),
      ns =>
        if (ns.isEmpty) List()
        else
          List(s"Negative cycles found: [${ns.map(s => s.map(_.toString).mkString(",")).mkString(",")}]")
    )
    val wellFormed = reasons.isEmpty
    SchemaInfo(name, "Iterative", wellFormed, reasons)
  }

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String] =
    for {
      schemaS <- shex2SLang(schema)
      program <- validate2Clingo(shapeMap, rdf, schemaS)
    } yield program.show

}

object ShExSchema {

  private def isActuallyValid: Boolean      = ???
  private def curatedErrors: Seq[ErrorInfo] = ???

  def empty: es.weso.schema.Schema = ShExSchema(schema = SchemaShEx.empty)

  def fromString(str: String, format: String, base: Option[String]): IO[ShExSchema] =
    RDFAsJenaModel.empty.flatMap(
      _.use(rdf =>
        for {
          schema <- SchemaShEx.fromString(str, format, base.map(IRI(_)), Some(rdf)).map(p => ShExSchema(p))
        } yield schema
      )
    )

}
