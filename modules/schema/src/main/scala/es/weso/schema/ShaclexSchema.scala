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
//import cats.data.EitherT
import cats.effect._

case class ShaclexSchema(schema: ShaclSchema) extends Schema {
  override def name = "SHACLex"

  override def formats: Seq[String] = DataFormats.formatNames ++ Seq("TREE")

  override def defaultTriggerMode: ValidationTrigger = TargetDeclarations

  override def validate(rdf: RDFReader, trigger: ValidationTrigger, builder: RDFBuilder): IO[Result] = trigger match {
    case TargetDeclarations => validateTargetDecls(rdf).map(_.addTrigger(trigger))
    case _ => IO(Result.errStr(s"Not implemented trigger ${trigger.name} for SHACL yet"))
  }

  def validateTargetDecls(rdf: RDFReader): IO[Result] = {
    val validator = Validator(schema)
    RDFAsJenaModel.empty.flatMap(_.use(emptyRdf => for {
      r <- validator.validateAll(rdf)
      builder <- emptyRdf.addPrefixMap(schema.pm)
      result <-  cnvResult(r, rdf, builder)
    } yield result))
  }

  def cnvResult(r: CheckResult[AbstractResult, (ShapeTyping,Boolean), List[Evidence]],
                rdf: RDFReader,
                builder: RDFBuilder
               ): IO[Result] = {
    val vr: ValidationReport =
      r.result.fold(
        e => ValidationReport.fromError(e), r =>
        r._1.toValidationReport
      )
    for {
      pm <- rdf.getPrefixMap
    } yield Result(
      isValid = vr.conforms,
      message = if (vr.conforms) "Valid" else "Not valid",
      shapeMaps = r.results.map(cnvShapeTyping(_, rdf,pm)),
      validationReport = ShaclexReport(vr),
      errors = vr.results.map(cnvViolationError),
      trigger = None,
      nodesPrefixMap = pm,
      shapesPrefixMap = schema.pm)
  }
  
  def cnvShapeTyping(t: (ShapeTyping, Boolean), rdf: RDFReader, pm: PrefixMap): ResultShapeMap = {
    ResultShapeMap(
      mapValues(t._1.getMap)(cnvMapShapeResult), pm, schema.pm)
  }

  private def cnvMapShapeResult(m: scala.collection.Map[Shape, TypingResult[AbstractResult, String]]): Map[ShapeMapLabel, Info] = {

    MapUtils.cnvMap(m.toMap, cnvShape, cnvTypingResult)
  }

  private def cnvShape(s: Shape): ShapeMapLabel = {
    s.id match {
      case iri: IRI => IRILabel(iri)
      case bnode: BNode => BNodeLabel(bnode)
      case _ => throw new Exception(s"cnvShape: unexpected ${s.id}")
    }
  }

  private def cnvTypingResult(t: TypingResult[AbstractResult, String]): Info = {
    import showShacl._
    import TypingResult.showTypingResult
    Info(
      status = if (t.isOK) Conformant else NonConformant,
      reason = Some(t.show)
    // TODO: Convert typing result to JSON and add it to appInfo
    )
  }

  private def cnvViolationError(v: AbstractResult): ErrorInfo = {
    val pm = schema.pm
    v match {
      case ar: MsgError => ErrorInfo(s"Error: $ar")
      case vr: ValidationResult =>
        ErrorInfo(
          pm.qualify(vr.sourceConstraintComponent) +
            " FocusNode: " + schema.pm.qualify(vr.focusNode) + " " +
            vr.message.mkString(","))
    }
  }

  /*def validateShapeMap(sm: Map[RDFNode,Set[String]], nodesStart: Set[RDFNode], rdf: RDFReader) : Result = {
    throw new Exception("Unimplemented validateShapeMap")
  }*/

  override def fromString(cs: CharSequence, format: String, 
     base: Option[String]
    ): IO[Schema] = {
    RDFAsJenaModel.fromString(cs.toString, format, base.map(IRI(_))).flatMap(_.use(rdf => for {
      eitherSchema <- RDF2Shacl.getShacl(rdf, resolveImports = true).attempt
      schema <- eitherSchema match {
        case Left(s) => IO.raiseError(new RuntimeException(s))
        case Right(schema) => IO.pure(schema)
      }
    } yield ShaclexSchema(schema)))
  }

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

  override def serialize(format: String, base: Option[IRI]): IO[String] = RDFAsJenaModel.empty.flatMap(_.use(builder => for {
    str <- if (formats.contains(format.toUpperCase))
      schema.serialize(format, base, builder)
    else IO.raiseError(new RuntimeException(s"Format $format not supported to serialize $name. Supported formats=$formats"))
  } yield str))

  override def empty: Schema = ShaclexSchema.empty

  override def shapes: List[String] = {
    schema.shapes.map(_.id).map(_.toString).toList
  }

  override def pm: PrefixMap = schema.pm

  override def convert(maybeTargetFormat: Option[String],
                       maybeTargetEngine: Option[String],
                       base: Option[IRI]
                      ): IO[String] = {
   val targetFormat = maybeTargetFormat.getOrElse(DataFormats.defaultFormatName)
   for {
    str <- maybeTargetEngine.map(_.toUpperCase) match {
     case None => 
       serialize(targetFormat)
     case Some("SHACL") | Some("SHACLEX") =>
       serialize(targetFormat)
     case Some("SHEX") => RDFAsJenaModel.empty.flatMap(_.use(builder => for {
       pair <- Shacl2ShEx.shacl2ShEx(schema).fold(
         s => IO.raiseError(new RuntimeException(s"SHACL2ShEx: Error converting: $s")),
         IO.pure
       )
       (newSchema,_) = pair
       str <- es.weso.shex.Schema.serialize(newSchema,targetFormat,base,builder)
     } yield (str)))
     case Some(other) =>
       IO.raiseError(new RuntimeException(s"Conversion $name -> $other not implemented yet"))
   } 
  } yield str
 }

  override def info: SchemaInfo = {
    // TODO: Check if shacl schemas are well formed
    SchemaInfo(name,"SHACLex", isWellFormed = true, List())
  }

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String] =
    IO.raiseError(new RuntimeException(s"Not implemented yet"))

}

object ShaclexSchema {
  def empty: ShaclexSchema = ShaclexSchema(schema = ShaclSchema.empty)

}
