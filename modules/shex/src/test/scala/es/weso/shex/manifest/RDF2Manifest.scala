package es.weso.shex.manifest

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.utils.FileUtils._
import ManifestPrefixes._
import es.weso.utils.EitherUtils._

import scala.util._

case class RDF2Manifest(base: Option[IRI],
                        derefIncludes: Boolean) extends RDFParser with LazyLogging {

  private def rdf2Manifest(rdf: RDFReader,
                   visited: List[RDFNode] = List()
                  ): Either[String, List[Manifest]] = for {
    candidates <- rdf.subjectsWithType(mf_Manifest)
    nodes <- parseNodes(candidates.toList, manifest(List()))(rdf)
  } yield nodes

  private def manifest(visited: List[IRI]): RDFParser[Manifest] = { (n, rdf) =>
    for {
      maybeLabel <- stringFromPredicateOptional(rdfs_label)(n, rdf)
      maybeComment <- stringFromPredicateOptional(rdfs_comment)(n, rdf)
      entries <- entries(n, rdf)
      includes <- includes(visited)(n, rdf)
    } yield {
      Manifest(
        label = maybeLabel,
        comment = maybeComment,
        entries = entries.toList,
        includes = includes)
    }
  }

  private def entries: RDFParser[Seq[Entry]] =
    parsePropertyList(mf_entries, entry)

  private def representationTest: RDFParser[RepresentationTest] = { (n,rdf) =>
    for {
    name <- stringFromPredicate(mf_name)(n, rdf)
    statusIri <- iriFromPredicate(mf_status)(n, rdf)
    shex <- iriFromPredicate(sx_shex)(n, rdf)
    ttl <- iriFromPredicate(sx_ttl)(n, rdf)
    json <- iriFromPredicate(sx_json)(n, rdf)
   } yield RepresentationTest(n,Status(statusIri),name,json,shex,ttl)
  }

  private def negativeSyntax: RDFParser[NegativeSyntax] = { (n,rdf) =>
    for {
      name <- stringFromPredicate(mf_name)(n, rdf)
      statusIri <- iriFromPredicate(mf_status)(n, rdf)
      shex <- iriFromPredicate(sx_shex)(n, rdf)
    } yield NegativeSyntax(n,Status(statusIri),name,shex)
  }

  private def negativeStructure: RDFParser[NegativeStructure] = { (n,rdf) =>
    for {
      name <- stringFromPredicate(mf_name)(n, rdf)
      statusIri <- iriFromPredicate(mf_status)(n, rdf)
      shex <- iriFromPredicate(sx_shex)(n, rdf)
    } yield NegativeStructure(n,Status(statusIri),name,shex)
  }

  private def validateTest: RDFParser[Validate] = { (n,rdf) =>
    for {
      name <- stringFromPredicate(mf_name)(n, rdf)
      statusIri <- iriFromPredicate(mf_status)(n, rdf)
      actionNode <- objectFromPredicate(mf_action)(n, rdf)
      action <- parseManifestAction(actionNode, rdf)
      resultNode <- objectFromPredicate(mf_result)(n, rdf)
      result <- result(resultNode, rdf)
      specRef <- optional(iriFromPredicate(sht_specRef))(n, rdf)
    } yield Validate(n,Status(statusIri),name,action,result,specRef)
  }

  private def validationTest: RDFParser[ValidationTest] = { (n,rdf) =>
    for {
      name <- stringFromPredicate(mf_name)(n, rdf)
      statusIri <- iriFromPredicate(mf_status)(n, rdf)
      traits <- irisFromPredicate(sht_trait)(n, rdf)
      comment <- stringFromPredicate(rdfs_comment)(n,rdf)
      actionNode <- objectFromPredicate(mf_action)(n, rdf)
      action <- parseAction(actionNode, rdf)
      maybeResult <- iriFromPredicateOptional(mf_result)(n,rdf)
    } yield ValidationTest(n,Status(statusIri),name,traits,comment,action,maybeResult)
  }

  private def validationFailure: RDFParser[ValidationFailure] = { (n,rdf) =>
    for {
      name <- stringFromPredicate(mf_name)(n, rdf)
      statusIri <- iriFromPredicate(mf_status)(n, rdf)
      traits <- irisFromPredicate(sht_trait)(n, rdf)
      comment <- stringFromPredicate(rdfs_comment)(n,rdf)
      actionNode <- objectFromPredicate(mf_action)(n, rdf)
      action <- parseAction(actionNode, rdf)
      maybeResult <- iriFromPredicateOptional(mf_result)(n,rdf)
    } yield ValidationFailure(n,Status(statusIri),name,traits,comment,action,maybeResult)
  }

  private def parseAction: RDFParser[Action] = { (n,rdf) => for {
    maybeFocus <- objectFromPredicateOptional(sht_focus)(n, rdf)
    action <- maybeFocus match {
      case Some(focus) => focusAction(focus)(n,rdf)
      case None => mapResultAction(n,rdf)
    }
   } yield action
  }

  private def mapResultAction: RDFParser[MapResultAction] = { (n,rdf) =>
    for {
      data <- iriFromPredicate(sht_data)(n, rdf)
      schema <- iriFromPredicate(sht_schema)(n, rdf)
      shapeMap <- iriFromPredicate(sht_map)(n, rdf)
    } yield MapResultAction(data,schema,shapeMap)
  }

  private def focusAction(focus: RDFNode): RDFParser[FocusAction] = { (n,rdf) =>
    for {
      schema <- iriFromPredicate(sht_schema)(n, rdf)
      data <- iriFromPredicate(sht_data)(n, rdf)
      shape <- objectFromPredicateOptional(sht_shape)(n, rdf)
//      maybeMap <- iriFromPredicateOptional(sht_map)(n,rdf)
      shapeExterns <- iriFromPredicateOptional(sht_shapeExterns)(n, rdf)
    } yield FocusAction(data,schema,focus,shape,shapeExterns)
  }

  private def entry: RDFParser[Entry] = { (n, rdf) =>
    for {
      entryTypeUri <- rdfType(n, rdf)
      entry <- entryTypeUri match {
        case `sht_RepresentationTest` => representationTest(n,rdf)
        case `sht_NegativeSyntax` => negativeSyntax(n,rdf)
        case `sht_NegativeStructure` => negativeStructure(n,rdf)
        case `sht_Validate` => validateTest(n,rdf)
        case `sht_ValidationTest`=> validationTest(n,rdf)
        case `sht_ValidationFailure`=> validationFailure(n,rdf)
        case _ => parseFail(s"Unsupported entry type: $entryTypeUri")
      }
    } yield entry
  }

  private def iriDataFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_TURTLE` => parseOk("TURTLE")
      case _ => parseFail("Unexpected schema format: " + iri)
    }
  }

  private def iriSchemaFormat2str(iri: IRI): Either[String, String] = {
    iri match {
      case `sht_SHACLC` => parseOk("SHACLC")
      case `sht_TURTLE` => parseOk("TURTLE")
      case _ => parseFail("Unexpected schema format: " + iri)
    }
  }

  private def parseManifestAction: RDFParser[ManifestAction] = { (n, rdf) =>
    for {
      data <- optional(iriFromPredicate(sht_data))(n, rdf)
      schema <- {
        iriFromPredicateOptional(sht_schema)(n, rdf)
      }
      dataFormatIri <- optional(iriFromPredicate(sht_data_format))(n, rdf)
      dataFormat <- mapOptional(dataFormatIri, iriDataFormat2str)
      schemaFormatIRI <- optional(iriFromPredicate(sht_schema_format))(n, rdf)
      schemaFormat <- mapOptional(schemaFormatIRI, iriSchemaFormat2str)
      schemaOutputFormat <- optional(iriFromPredicate(sht_schema_output_format))(n, rdf)
      triggerMode <- optional(iriFromPredicate(sht_triggerMode))(n, rdf)
      node <- optional(oneOfPredicates(Seq(sht_node, sht_focus)))(n, rdf)
      shape <- optional(iriFromPredicate(sht_shape))(n, rdf)
      focus <- optional(iriFromPredicate(sht_focus))(n, rdf)
      shapeMap <- optional(iriFromPredicate(sht_shapeMap))(n, rdf)
      resultShapeMap <- optional(iriFromPredicate(sht_resultShapeMap))(n, rdf)
    } yield ManifestAction(
      schema = schema,
      schemaFormat = schemaFormat,
      data = data,
      dataFormat = dataFormat,
      triggerMode = triggerMode,
      schemaOutputFormat = schemaOutputFormat,
      node = node,
      shape = shape,
      shapeMap = shapeMap,
      focus = focus,
      resultShapeMap = resultShapeMap
    )
  }

  private def result: RDFParser[Result] = { (n, rdf) =>
    {
      n match {
        case BooleanLiteral(b) => Right(BooleanResult(b))
        case iri: IRI =>
          if (noType(iri, rdf)) Right(IRIResult(iri))
          else compoundResult(iri, rdf)
        case bNode: BNode => compoundResult(bNode, rdf)
        case _ => parseFail("Unexpected type of result " + n)
      }
    }
  }

  private def compoundResult: RDFParser[Result] = (n, rdf) => {
    iriFromPredicate(rdf_type)(n, rdf) match {
      case Right(iri) => iri match {
        case `sht_ResultShapeMap` => for {
          iri <- iriFromPredicate(`sht_resultShapeMap`)(n, rdf)
        } yield ResultShapeMapIRI(iri)
        case _ => parseFail(s"Unsupporte type of compound result: $iri")
      }
      case Left(e) => parseFail(s"compoundResult. Wrong rdf:type of node: $n: $e")
    }
  }

/*  private def validatedPairs: RDFParser[Set[ValidPair]] =
    parsePropertyValues(sht_pair, parsePair)

  private def parsePair: RDFParser[ValidPair] = (n, rdf) => for {
    node <- objectFromPredicate(sht_node)(n, rdf)
    shape <- objectFromPredicate(sht_shape)(n, rdf)
  } yield ValidPair(node, shape) */

  private def noType(n: RDFNode, rdf: RDFReader): Boolean = {
    val types = objectsFromPredicate(rdf_type)(n, rdf)
    types.getOrElse(Set()).isEmpty
  }

  private def includes(visited: List[RDFNode]):
     RDFParser[List[(RDFNode, Manifest)]] = { (n, rdf) => {
    if (derefIncludes) {
      for {
        includes <- {
          objectsFromPredicate(mf_include)(n, rdf)
        }
        result <- {
          val ds: List[Either[String, (IRI, Manifest)]] =
            includes.toList.map(iri => derefInclude(iri, base, iri +: visited))
          sequence(ds)
        }
      } yield
        result
    }
    else parseOk(List())
  }
  }

  /* TODO: The following code doesn't take into account possible loops */
  private def derefInclude(node: RDFNode,
                   base: Option[IRI],
                   visited: List[RDFNode]): Either[String,(IRI,Manifest)] = node match {
    case iri: IRI => {
      val iriResolved = base.fold(iri)(base => base.resolve(iri))
      for {
        rdf <- RDFAsJenaModel.fromURI(iriResolved.getLexicalForm,"TURTLE",None)
        mfs <- RDF2Manifest(Some(iriResolved), true).rdf2Manifest(rdf, iri +: visited)
        manifest <- if (mfs.size == 1) Right(mfs.head)
                    else Left(s"More than one manifests found: ${mfs} at iri $iri")
      } yield (iri, manifest)
    }
    case _ => Left(s"Trying to deref an include from node $node which is not an IRI")
  }

/*  private def parsePropertyValue[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = (n, rdf) => for {
    value <- objectFromPredicate(pred)(n, rdf)
    result <- parser(value, rdf)
  } yield result

  private def parsePropertyValues[A](pred: IRI, parser: RDFParser[A]): RDFParser[Set[A]] = (n, rdf) => for {
    values <- objectsFromPredicate(pred)(n, rdf)
    results <- parseNodes(values.toList, parser)(rdf)
  } yield results.toSet */

  private def parsePropertyList[A](
    pred: IRI,
    parser: RDFParser[A]): RDFParser[List[A]] = (n, rdf) => for {
    ls <- rdfListForPredicateAllowingNone(pred)(n, rdf)
    vs <- parseNodes(ls, parser)(rdf)
  } yield vs

  private def mapOptional[A, B](optA: Option[A], fn: A => Either[String, B]): Either[String, Option[B]] = {
    optA match {
      case None => parseOk(None)
      case Some(x) => {
        fn(x) match {
          case Right(v) => Right(Some(v))
          case Left(e) => Left(e)
        }
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
  override def objectFromPredicate(p: IRI): RDFParser[RDFNode] = { (n, rdf) =>
    for {
      ts <- rdf.triplesWithSubjectPredicate(n, p)
      r <- ts.size match {
        case 0 =>
          parseFail(
            s"objectFromPredicate: Not found triples with subject $n and predicate $p \nRDF: ${rdf.serialize("TURTLE")}")
        case 1 => parseOk(ts.head.obj)
        case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
      }
    } yield r
  }

}

object RDF2Manifest extends LazyLogging {

  def read(fileName: String,
           format: String,
           base: Option[String],
           derefIncludes: Boolean
          ): Either[String, Manifest] = {
    for {
      cs <- getContents(fileName)
      rdf <- RDFAsJenaModel.fromChars(cs, format, base.map(IRI(_)))
      iriBase <- base match {
        case None => Right(None)
        case Some(str) => IRI.fromString(str).map(Some(_))
      }
      mfs <- {
        RDF2Manifest(iriBase, derefIncludes).rdf2Manifest(rdf)
      }
      manifest <- if (mfs.size == 1) Right(mfs.head)
      else Left(s"Number of manifests != 1: ${mfs}")
    } yield manifest
  }

}
