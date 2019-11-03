package es.weso.shacl.manifest

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.utils.FileUtils._
import ManifestPrefixes._
import es.weso.utils.EitherUtils._
import scala.util._
import cats.implicits._

case class RDF2Manifest(base: Option[IRI],
                        derefIncludes: Boolean) extends RDFParser with LazyLogging {

  def rdf2Manifest(rdf: RDFReader, visited: List[RDFNode] = List()): Either[String, List[Manifest]] =
    for {
      candidates <- rdf.subjectsWithType(mf_Manifest)
      cfg = Config(IRI("http://internal/base"), rdf)
      ns         <- parseNodes(candidates.toList, manifest(List())).value.run(cfg)
      // parseNodes(candidates.toList, manifest(List()))(rdf)
    } yield ns

  def manifest(visited: List[IRI]): RDFParser[Manifest] =  
    for {
      maybeLabel   <- stringFromPredicateOptional(rdfs_label)
      maybeComment <- stringFromPredicateOptional(rdfs_comment)
      entries      <- entries
      includes     <- includes(visited)
    } yield {
      Manifest(label = maybeLabel, comment = maybeComment, entries = entries.toList, includes = includes)
    }

  def entries: RDFParser[List[Entry]] =
    parsePropertyList(mf_entries, entry)

  def getEntryType(node: RDFNode): Either[String, EntryType] = {
    node match {
      case `sht_Validate`            => Right(Validate)
      case `sht_ValidationFailure`   => Right(ValidationFailure)
      case `sht_MatchNodeShape`      => Right(MatchNodeShape)
      case `sht_WellFormedSchema`    => Right(WellFormedSchema)
      case `sht_NonWellFormedSchema` => Right(NonWellFormedSchema)
      case `sht_ConvertSchemaSyntax` => Right(ConvertSchemaSyntax)
      case _                         => Left("Unexpected entry type: " + node)
    }
  }

  def entry: RDFParser[Entry] = for {
      n <- getNode
      entryTypeUri <- rdfType
      entryType    <- fromEither(getEntryType(entryTypeUri))
      maybeName    <- stringFromPredicateOptional(mf_name)
      actionNode   <- objectFromPredicate(mf_action)
      action       <- withNode(actionNode, action) 
      resultNode   <- objectFromPredicate(mf_result)
      result       <- withNode(resultNode, result) 
      statusIri    <- iriFromPredicate(mf_status)
      specRef      <- optional(iriFromPredicate(sht_specRef))
    } yield
      Entry(node = n,
            entryType = entryType,
            name = maybeName,
            action = action,
            result = result,
            status = Status(statusIri),
            specRef = specRef)

  def iriDataFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_TURTLE` => Right("TURTLE")
      case _            => Left("Unexpected schema format: " + iri)
    }
  }

  def iriSchemaFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_SHACLC` => Right("SHACLC")
      case `sht_TURTLE` => Right("TURTLE")
      case _            => Left(s"Unexpected schema format: $iri")
    }
  }

  private def action: RDFParser[ManifestAction] = 
    for {
      data <- optional(iriFromPredicate(sht_dataGraph))
      schema <- iriFromPredicateOptional(sht_shapesGraph)
      dataFormatIri      <- optional(iriFromPredicate(sht_data_format))
      dataFormat         <- mapOptional(dataFormatIri, iriDataFormat2str)
      schemaFormatIRI    <- optional(iriFromPredicate(sht_schema_format))
      schemaFormat       <- mapOptional(schemaFormatIRI, iriSchemaFormat2str)
      schemaOutputFormat <- optional(iriFromPredicate(sht_schema_output_format))
      triggerMode        <- optional(iriFromPredicate(sht_triggerMode))
      node               <- optional(oneOfPredicates(Seq(sht_node, sht_focus)))
      shape              <- optional(iriFromPredicate(sht_shape))
    } yield
      ManifestAction(
        schema = schema,
        schemaFormat = schemaFormat,
        data = data,
        dataFormat = dataFormat,
        triggerMode = triggerMode,
        schemaOutputFormat = schemaOutputFormat,
        node = node,
        shape = shape
      )

  private def result: RDFParser[Result] = for { 
    n <- getNode
    v <- n match {
        case BooleanLiteral(b) => ok(BooleanResult(b))
        case iri: IRI => 
         for { 
           b <- noType
           v <- if (b) { 
                  val r: RDFParser[Result] = ok(IRIResult(iri)) 
                  r
                }
                else compoundResult
         } yield v 
        case bNode: BNode => compoundResult
        case _            => parseFail("Unexpected type of result " + n)
      }
   } yield v


  private def compoundResult: RDFParser[Result] = for {
    n <- getNode
    maybeType <- optional(iriFromPredicate(rdf_type))
    v <- maybeType match {
      case None => parseFail(s"compoundResult. No rdf:type for node: $n")
      case Some(`sh_ValidationReport`) => for {
        report <- validationReport
      } yield ReportResult(report)
      case Some(other) => parseFail(s"compoundResult. rdf:type for node $n should be ${`sh_ValidationReport`}")
    }
  } yield v
  
  private def validationReport: RDFParser[ValidationReport] =
    parsePropertyValues(sh_result, violationError).map(ValidationReport(_))

  private def violationError: RDFParser[ViolationError] =
    for {
        errorType   <- optional(iriFromPredicate(rdf_type))
        focusNode   <- optional(objectFromPredicate(sh_focusNode))
        path        <- optional(iriFromPredicate(sh_path))
        severity    <- optional(iriFromPredicate(sh_severity))
        scc         <- optional(iriFromPredicate(sh_sourceConstraintComponent))
        sourceShape <- optional(iriFromPredicate(sh_sourceShape))
        value       <- optional(objectFromPredicate(sh_value))
      } yield {
        ViolationError(errorType, focusNode, path, severity, scc, sourceShape, value)
    }

  private def noType: RDFParser[Boolean] = for {
    types <- objectsFromPredicate(rdf_type)
  } yield types.isEmpty

  private def includes(visited: List[RDFNode]): RDFParser[List[(RDFNode, Option[Manifest])]] = 
      for {
        includes <- objectsFromPredicate(mf_include)
        result <- {
          val ds: List[RDFParser[(IRI, Option[Manifest])]] =
            includes.toList.map(iri => derefInclude(iri, base, iri +: visited))
          ds.sequence
        }
      } yield result

  /* TODO: The following code doesn't take into account possible loops */
  private def derefInclude(node: RDFNode, base: Option[IRI], visited: List[RDFNode],
  ): RDFParser[(IRI, Option[Manifest])] = node match {
    case iri: IRI =>
      if (derefIncludes) {
        val iriResolved = base.fold(iri)(base => base.resolve(iri))
        for {
          rdf <- fromEither(RDFAsJenaModel.fromURI(iriResolved.getLexicalForm, "TURTLE", Some(iriResolved)))
          mfs <- fromEither(RDF2Manifest(Some(iriResolved), true).rdf2Manifest(rdf, iri +: visited))
          manifest <- if (mfs.size == 1) ok(mfs.head)
          else parseFail(s"More than one manifests found: ${mfs} at iri $iri")
        } yield (iri, Some(manifest))
      } else ok((iri, None))
    case _ => parseFail(s"Trying to deref an include from node $node which is not an IRI")
  }

  private def parsePropertyValues[A](pred: IRI, parser: RDFParser[A]): RDFParser[Set[A]] =
    for {
        values  <- objectsFromPredicate(pred)
        results <- parseNodes(values.toList, parser)
      } yield results.toSet

  private def parsePropertyList[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] =
    for {
        ls <- rdfListForPredicateAllowingNone(pred)
        vs <- parseNodes(ls, parser)
    } yield vs

  private def mapOptional[A, B](optA: Option[A], fn: A => Either[String, B]): RDFParser[Option[B]] = {
    optA match {
      case None => ok(None)
      case Some(x) => {
        fromEither(fn(x).map(_.some))
      }
    }
  }

  def oneOfPredicates(predicates: Seq[IRI]): RDFParser[IRI] = {
    val ps = predicates.map(iriFromPredicate(_))
    oneOf(ps)
  }

  /**
   * Override this method to provide more info
   */
  override def objectFromPredicate(p: IRI): RDFParser[RDFNode] = 
    for {
      rdf <- getRDF
      n <- getNode
      ts <- fromEither(rdf.triplesWithSubjectPredicate(n, p))
      r <- ts.size match {
        case 0 =>
          parseFail(
            s"objectFromPredicate: Not found triples with subject $n and predicate $p \nRDF: ${rdf.serialize("TURTLE")}")
        case 1 => parseOk(ts.head.obj)
        case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
      }
    } yield r
}

object RDF2Manifest extends LazyLogging {

  def read(fileName: String,
           format: String,
           base: Option[String],
           derefIncludes: Boolean
          ): Either[String, (Manifest,RDFBuilder)] = {
    for {
      cs <- getContents(fileName)
      iriBase <- base match {
        case None => Right(None)
        case Some(str) => IRI.fromString(str).map(Some(_))
      }
      rdf <- RDFAsJenaModel.fromChars(cs, format, iriBase).map(_.normalizeBNodes)
      _ <- { println(s"RDF: ${rdf.serialize("TURTLE")}\n---");Right(())}
      manifest <- fromRDF(rdf, iriBase, derefIncludes)
      _ <- { println(s"Manifest: ${manifest.toString}\n---");Right(())}
    } yield (manifest,rdf)
  }

  def fromRDF(rdf: RDFReader, base: Option[IRI], derefIncludes: Boolean): Either[String, Manifest] = {
    RDF2Manifest(base,derefIncludes).rdf2Manifest(rdf).
      flatMap(takeSingle(_,"Number of manifests != 1"))
  }

}
