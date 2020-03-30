package es.weso.schema

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shapeMaps._
import es.weso.shex.converter.ShEx2Shacl
import es.weso.shex.shexR._
import es.weso.shex.validator.{Result => _, _}
import es.weso.shex.{Schema => SchemaShEx}
import es.weso.slang.{SLang2Clingo, ShEx2SLang}
import cats.effect._
import cats.data.EitherT
import scala.util._
import es.weso.shex.ResolvedSchema

case class ShExSchema(schema: SchemaShEx)
    extends es.weso.schema.Schema
    with LazyLogging
    with SLang2Clingo
    with ShEx2SLang {
  override def name = "ShEx"

  lazy val shExCFormat = "ShExC"
  lazy val shExJFormat = "ShExJ"
  //  lazy val svgFormat = "SVG"

  def getValidator(): IO[Validator] = for { 
    resolvedSchema <- ResolvedSchema.resolve(schema, None)
  } yield Validator(resolvedSchema)

  // TODO: Separate input/output formats
  override def formats =
    List(shExCFormat, shExJFormat) ++
      RDFAsJenaModel.availableFormats
  // ++ List(svgFormat)

  lazy val formatsUpperCase = formats.map(_.toUpperCase)

  override def defaultTriggerMode: ValidationTrigger = ShapeMapTrigger.empty

  override def validate(rdf: RDFReader, trigger: ValidationTrigger): IO[Result] =
    (trigger match {
      case TargetDeclarations => validateTargetDecls(rdf)
      //    case MapTrigger(sm, ns) => validateShapeMap(sm, ns, rdf)
      case ShapeMapTrigger(sm) => for {
        eitherFixedMap <- ShapeMap.fixShapeMap(sm, rdf, rdf.getPrefixMap(), schema.prefixes.getOrElse(PrefixMap.empty)).attempt
        result <- eitherFixedMap match {
          case Left(e) => IO(Result.errStr(s"Error fixing shape map: $e"))
          case Right(fixedShapeMap) => validateFixedShapeMap(fixedShapeMap, rdf)
        } 
      } yield result
    }).map(_.addTrigger(trigger))

  def validateTargetDecls(rdf: RDFReader): IO[Result] = for {
    validator <- getValidator()
    r <- validator.validateNodeDecls(rdf)
    converted = cnvResult(r.toEitherS, rdf)
  } yield converted

  def cnvResult(r: Either[String, ResultShapeMap], rdf: RDFReader): Result = r match {
    case Left(msg) =>
      Result(
        false,
        message = "Error validating",
        shapeMaps = Seq(),
        validationReport = Left("No validation report in ShEx"),
        errors = Seq(ErrorInfo(msg)),
        None,
        rdf.getPrefixMap(),
        schema.prefixMap
      )
    case Right(resultShapeMap) =>
      Result(
        true,
        "Validated",
        shapeMaps = Seq(resultShapeMap),
        validationReport = Left(s"No validaton report in ShEx"),
        errors = Seq(),
        None,
        rdf.getPrefixMap(),
        schema.prefixMap
      )
  }

  def validateFixedShapeMap(fixedShapeMap: FixedShapeMap, rdf: RDFReader): IO[Result] = {
    validateShapeMap(fixedShapeMap, rdf)
  }

  def validateNodeShape(node: IRI, shape: String, rdf: RDFReader): IO[Result] = for {
    validator <- getValidator
    r <- validator.validateNodeShape(rdf, node, shape)
    res = cnvResult(r.toEitherS, rdf)
  } yield res

  def validateNodeStart(node: IRI, rdf: RDFReader): IO[Result] = for {
    validator <- getValidator
    r <- validator.validateNodeStart(rdf, node)
    res = cnvResult(r.toEitherS, rdf)
  } yield res


  def validateShapeMap(shapeMap: FixedShapeMap, rdf: RDFReader): IO[Result] = for {
    validator <- getValidator
    r <- validator.validateShapeMap(rdf,shapeMap).attempt
    res <- r match {
      case Left(error) =>
        IO(Result(
          false,
          "Error validating",
          Seq(),
          Left("No validation report yet"),
          Seq(ErrorInfo(error.getMessage())),
          None,
          rdf.getPrefixMap(),
          schema.prefixMap
        ))
      case Right(result) => for {
        resultShapeMap <- result.toResultShapeMap
      } yield Result(
          true,
          "Validated",
          Seq(resultShapeMap),
          Left(s"No validation report for ShEx"),
          Seq(),
          None,
          rdf.getPrefixMap(),
          schema.prefixMap
        )
      }
  } yield res 
/*    match {
      case Right(resultShapeMap) => {
        // println(s"Validated, result=$resultShapeMap")
        Result(
          true,
          "Validated",
          Seq(resultShapeMap),
          Left(s"No validation report for ShEx"),
          Seq(),
          None,
          rdf.getPrefixMap(),
          schema.prefixMap
        )
      }
    }
  } */

  def cnvViolationError(v: ShExError): ErrorInfo = {
    ErrorInfo(v.show)
  }

  override def fromString(cs: CharSequence, format: String, base: Option[String]): EitherT[IO, String, Schema] = {
    EitherT.liftF(ShExSchema.fromString(cs, format, base))
  }

  override def fromRDF(rdf: RDFReader): EitherT[IO, String, es.weso.schema.Schema] = {
    val e: EitherT[IO, String, SchemaShEx] = RDF2ShEx.rdf2Schema(rdf)
    e.map(s => {
      val schema: es.weso.schema.Schema = ShExSchema(s)
      schema
    })
  }

  override def serialize(format: String, base: Option[IRI]): IO[String] = {
    val fmt                 = format.toUpperCase
    for {
      builder <- RDFAsJenaModel.empty
      str <- fmt.toUpperCase match {
          case _ if (formatsUpperCase.contains(fmt)) => SchemaShEx.serialize(schema, fmt, base, builder)
          case _ => IO.raiseError(new RuntimeException(s"Can't serialize to format $format. Supported formats=$formats"))
      } 
    } yield str
  }

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
  ): EitherT[IO, String, String] = {
    targetEngine.map(_.toUpperCase) match {
      case None => EitherT.liftF(serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base))
      case Some(engine) if (engine.equalsIgnoreCase(name)) => {
        EitherT.liftF(serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base))
      }
      case Some("SHACL") | Some("SHACLEX") =>
        for {
          newSchema <- EitherT.fromEither[IO](ShEx2Shacl.shex2Shacl(schema, None).leftMap(es => es.mkString("\n")))
          builder <- EitherT.liftF(RDFAsJenaModel.empty)
          // TODO: Check if we should pass base
          str <- EitherT.liftF(newSchema.serialize(targetFormat.getOrElse(DataFormats.defaultFormatName), base, builder))
        } yield str
      case Some(unknown) => EitherT.fromEither(Left(s"Conversion from ShEx to $unknown not implemented yet"))
    }
  }

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

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): EitherT[IO,String,String] =
    for {
      schemaS <- shex2SLang(schema)
      // _ <- {println(s"SchemaS: $schemaS"); Right(())}
      program <- EitherT.fromEither[IO](validate2Clingo(shapeMap, rdf, schemaS))
      // _ <- {println(s"Program: $program"); Right(())}
    } yield program.show

}

object ShExSchema {
  def empty: es.weso.schema.Schema = ShExSchema(schema = SchemaShEx.empty)

  def fromString(cs: CharSequence, format: String, base: Option[String]): IO[ShExSchema] = for {
    rdf <- RDFAsJenaModel.empty
    schema <- SchemaShEx.fromString(cs, format, base.map(IRI(_)), Some(rdf)).map(p => ShExSchema(p))
  } yield schema

}
