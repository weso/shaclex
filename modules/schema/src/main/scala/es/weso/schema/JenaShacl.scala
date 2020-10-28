package es.weso.schema

import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.SHACLPrefixes.`owl:imports`
import es.weso.shacl.report.{AbstractResult, MsgError}
import es.weso.shacl.{Schema => ShaclSchema, _}
// import es.weso.shacl._
import es.weso.shacl.converter.{RDF2Shacl, Shacl2ShEx}
import es.weso.shacl.report.{ValidationReport, ValidationResult}
import es.weso.shacl.validator.{CheckResult, Evidence, ShapeTyping, Validator}
import es.weso.shapeMaps._
import es.weso.utils.internal.CollectionCompat._
import util._
import es.weso.typing._
import es.weso.utils.MapUtils
import cats.data.EitherT
import cats.effect._
import org.apache.jena.shacl.Shapes
import scala.util.control.NoStackTrace
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory

case class JenaShaclException(msg: String) extends Exception(msg) with NoStackTrace


case class JenaShacl(shapesGraph: Model) extends Schema {
  override def name = "JenaSHACL"

  override def formats: Seq[String] = DataFormats.formatNames

  override def defaultTriggerMode: ValidationTrigger = TargetDeclarations

  override def validate(rdf: RDFReader, trigger: ValidationTrigger, builder: RDFBuilder): IO[Result] = trigger match {
    case TargetDeclarations => validateTargetDecls(rdf).map(_.addTrigger(trigger))
    case _ => IO(Result.errStr(s"Not implemented trigger ${trigger.name} for SHACL yet"))
  }

  def validateTargetDecls(rdf: RDFReader): IO[Result] = rdf match {
    case _: RDFAsJenaModel => {
      IO.raiseError(JenaShaclException(s"Not Implemented yet for RDFAsJenaModel"))
    }
    case _ => IO.raiseError(JenaShaclException(s"Not Implemented Jena SHACL validation for ${rdf.rdfReaderName} yet"))
  }
    /*for {
     r <- validator.validateAll(rdf)
     emptyRdf <- RDFAsJenaModel.empty  
     builder <- emptyRdf.addPrefixMap(schema.pm)
     result <-  cnvResult(r, rdf, builder)
    } yield result */

  def cnvResult(r: CheckResult[AbstractResult, (ShapeTyping,Boolean), List[Evidence]],
                rdf: RDFReader,
                builder: RDFBuilder
               ): IO[Result] = {
    val vr: ValidationReport =
      r.result.fold(e => ValidationReport.fromError(e), r =>
        r._1.toValidationReport
      )
    for {
      eitherVR <- vr.toRDF(builder).attempt
      pm <- rdf.getPrefixMap
    } yield Result(
      isValid = vr.conforms,
      message = if (vr.conforms) "Valid" else "Not valid",
      shapeMaps = ???,
      validationReport = eitherVR.leftMap(_.getMessage),
      errors = ???,
      trigger = None,
      nodesPrefixMap = pm,
      shapesPrefixMap = PrefixMap.empty
      )
  }
  
  override def fromString(cs: CharSequence, format: String, 
     base: Option[String]
    ): IO[Schema] = IO.raiseError(JenaShaclException(s"Not implemented yet"))

  // private def err[A](msg:String): EitherT[IO,String, A] = EitherT.leftT[IO,A](msg)

  override def fromRDF(rdf: RDFReader): IO[es.weso.schema.Schema] = for {
    eitherBuilder <- rdf.asRDFBuilder.attempt
    schema <- eitherBuilder match {
    case Left(_) => for {
      ts <- rdf.triplesWithPredicate(`owl:imports`).compile.toList
      schema <- ts.size match {
        case 0 => RDF2Shacl.getShaclReader(rdf).map(ShaclexSchema(_))
        case _ => IO.raiseError(new RuntimeException(s"fromRDF: Not supported owl:imports for this kind of RDF model\nRDFReader: $rdf"))
      }
    } yield schema

    case Right(rdfBuilder) =>
      for {
        maybeSchemaShacl <- RDF2Shacl.getShacl(rdfBuilder, resolveImports = true).attempt
        schemaShacl <- maybeSchemaShacl.fold(
          s => IO.raiseError(new RuntimeException(s)),
          s => IO.pure(s)
        )
      } yield {
        val ss: es.weso.schema.Schema = ShaclexSchema(schemaShacl)
        ss
      }
   }
  } yield schema

/*  private def handleErr[A](e: Either[String,A]): IO[A] = e.fold(
    s => IO.raiseError(new RuntimeException(s)),
    IO.pure
  ) */

  override def serialize(format: String, base: Option[IRI]): IO[String] = for {
    builder <- RDFAsJenaModel.empty
    str <- if (formats.contains(format.toUpperCase))
      IO.raiseError(JenaShaclException(s"not implemented serialize yet"))
    else IO.raiseError(JenaShaclException(s"Format $format not supported to serialize $name. Supported formats=$formats"))
  } yield str  

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    List()
  }

  override def pm: PrefixMap = ???

  override def convert(targetFormat: Option[String],
                       targetEngine: Option[String],
                       base: Option[IRI]
                      ): IO[String] = {
   targetEngine.map(_.toUpperCase) match {
     case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some("SHACL") | Some("SHACLEX") =>
       serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some("SHEX") => ??? /*for {
       pair <- Shacl2ShEx.shacl2ShEx(schema).fold(
         s => IO.raiseError(new RuntimeException(s"SHACL2ShEx: Error converting: $s")),
         IO.pure
       )
       (newSchema,_) = pair
       builder <- RDFAsJenaModel.empty
       str <- es.weso.shex.Schema.serialize(
         newSchema,
         targetFormat.getOrElse(DataFormats.defaultFormatName),
         base,
         builder)
     } yield str */
     case Some(other) =>
       IO.raiseError(JenaShaclException(s"Conversion $name -> $other not implemented yet"))
   }
  }

  override def info: SchemaInfo = {
    // TODO: Check if shacl schemas are well formed
    SchemaInfo(name,"SHACLex", isWellFormed = true, List())
  }

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String] =
    IO.raiseError(JenaShaclException(s"Not implemented yet toClingo for $name"))

}

object JenaShacl {
  def empty: JenaShacl = {
    val m = ModelFactory.createDefaultModel()
    JenaShacl(m)
  }

}
