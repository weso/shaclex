package es.weso.manifest

import es.weso.rdf.nodes._
import es.weso.rdf._
import scala.util._
import es.weso.rdf._
import ManifestPrefixes._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.PrefixMap
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.jena.RDFAsJenaModel
import java.io.File
import es.weso.utils.FileUtils._
import es.weso.utils.TryUtils._


case class RDF2ManifestException(msg:String)
 extends Exception(msg)

trait RDF2Manifest
  extends RDFParser {

  def rdf2Manifest(
     rdf: RDFReader,
     derefIncludes: Boolean
     ): Either[String, List[Manifest]] = {
    val candidates = subjectsWithType(mf_Manifest,rdf).toList
    parseNodes(candidates, manifest(derefIncludes))(rdf)
 }

 def manifest(derefIncludes: Boolean): RDFParser[Manifest] = { (n,rdf) =>
   for {
     maybeLabel <- stringFromPredicateOptional(rdfs_label)(n,rdf)
     maybeComment <- stringFromPredicateOptional(rdfs_comment)(n,rdf)
     entries <- entries(n,rdf)
     includes <- includes(derefIncludes)(n,rdf)
   } yield
      Manifest(
          label = maybeLabel,
          comment = maybeComment,
          entries = entries.toList,
          includes = includes
          )
 }

 def entries:RDFParser[Seq[Entry]] =
   parsePropertyList(mf_entries, entry)

 def getEntryType(node: RDFNode): Either[String,EntryType] = {
   node match {
     case `sht_Validate` => Right(Validate)
     case `sht_ValidationTest` => Right(ValidationTest)
     case `sht_ValidationFailure` => Right(ValidationFailure)
     case `sht_MatchNodeShape` => Right(MatchNodeShape)
     case `sht_WellFormedSchema` => Right(WellFormedSchema)
     case `sht_NonWellFormedSchema` => Right(NonWellFormedSchema)
     case `sht_ConvertSchemaSyntax` => Right(ConvertSchemaSyntax)
     case _ => Left("Unexpected entry type: " + node)
   }
 }

 def entry: RDFParser[Entry] = { (n,rdf) =>
   for {
     entryTypeUri <- rdfType(n,rdf)
     entryType <- getEntryType(entryTypeUri)
     name <- stringFromPredicate(mf_name)(n,rdf)
     actionNode <- objectFromPredicate(mf_action)(n,rdf)
     action <- action(actionNode,rdf)
     resultNode <- objectFromPredicate(mf_result)(n,rdf)
     result <- result(resultNode,rdf)
     statusIri <- iriFromPredicate(mf_status)(n,rdf)
     specRef <- optional(iriFromPredicate(sht_specRef))(n,rdf)
   } yield
     Entry(node = n,
      entryType = entryType,
      name = name,
      action = action,
      result = result,
      status = Status(statusIri),
      specRef = specRef
     )
 }


 def iriDataFormat2str(iri: IRI): Either[String,String] = {
   iri match {
     case `sht_TURTLE` => parseOk("TURTLE")
     case _ => parseFail("Unexpected schema format: " + iri)
   }
 }

 def iriSchemaFormat2str(iri: IRI): Either[String,String] = {
   iri match {
     case `sht_SHACLC` => parseOk("SHACLC")
     case `sht_TURTLE` => parseOk("TURTLE")
     case _ => parseFail("Unexpected schema format: " + iri)
   }
 }


 def action: RDFParser[ManifestAction] = { (n,rdf) =>
   for {
     data <- optional(iriFromPredicate(sht_data))(n,rdf)
     schema <- iriFromPredicateOptional(sht_schema)(n,rdf)
     dataFormatIri <- optional(iriFromPredicate(sht_data_format))(n,rdf)
     dataFormat <- mapOptional(dataFormatIri,iriDataFormat2str)
     schemaFormatIRI <- optional(iriFromPredicate(sht_schema_format))(n,rdf)
     schemaFormat <- mapOptional(schemaFormatIRI,iriSchemaFormat2str)
     schemaOutputFormat <- optional(iriFromPredicate(sht_schema_output_format))(n,rdf)
     triggerMode <- optional(iriFromPredicate(sht_triggerMode))(n,rdf)
     node <- optional(oneOfPredicates(Seq(sht_node,sht_focus)))(n,rdf)
     shape <- optional(iriFromPredicate(sht_shape))(n,rdf)
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
 }

/* def maybeResult: (Option[RDFNode],RDFReader) => Try[Result] = { (m,rdf) =>{
   println(s"Parsing maybe result...$m")
   m match {
     case None => Success(EmptyResult)
     case Some(resultNode) => result(resultNode,rdf)
   }
  }
 } */

 def result: RDFParser[Result] = { (n,rdf) => {
   n match {
     case BooleanLiteral(b) => Right(BooleanResult(b))
     case iri:IRI =>
       if (noType(iri,rdf)) Right(IRIResult(iri))
       else compoundResult(iri,rdf)
     case bNode: BNodeId => compoundResult(bNode,rdf)
     case _ => parseFail("Unexpected type of result " + n)
   }
  }
 }

 def compoundResult: RDFParser[Result] = (n,rdf) => {
  iriFromPredicate(rdf_type)(n,rdf) match {
    case Right(iri) => iri match {
      case `sht_NotValid` => for {
        detailsNode <- objectFromPredicate(sht_details)(n,rdf)
        validationReport <- parsePropertyValue(sht_validationReport,validationReport)(detailsNode, rdf)
        validatedPairs <- parsePropertyValue(sht_validatedPairs, validatedPairs)(detailsNode, rdf)
        } yield NotValidResult(validationReport, validatedPairs)
      case `sht_Valid` => Right(ValidResult(List()))
      case _ => parseFail(s"Unsupporte type of compound result: $iri")
    }
    case Left(e) => parseFail(s"compoundResult. Wrong rdf:type of node: $n: $e")
  }
 }

 def validatedPairs: RDFParser[Set[ValidPair]] =
   parsePropertyValues(sht_pair, parsePair)

 def parsePair: RDFParser[ValidPair] = (n,rdf) => for {
   node <- objectFromPredicate(sht_node)(n,rdf)
   shape <- objectFromPredicate(sht_shape)(n,rdf)
 } yield ValidPair(node,shape)

 def validationReport: RDFParser[ValidationReport] = (n,rdf) =>
   parsePropertyValues(sh_result, violationError)(n,rdf).
   map(ValidationReport(_))

 def violationError: RDFParser[ViolationError] = (n,rdf) => for {
   errorType <- optional(iriFromPredicate(rdf_type))(n,rdf)
   focusNode <- optional(iriFromPredicate(sh_focusNode))(n,rdf)
   path <- optional(iriFromPredicate(sh_path))(n,rdf)
   severity <- optional(iriFromPredicate(sh_severity))(n,rdf)
   scc <- optional(iriFromPredicate(sh_sourceConstraintComponent))(n,rdf)
   sourceShape <- optional(iriFromPredicate(sh_sourceShape))(n,rdf)
   value <- optional(objectFromPredicate(sh_value))(n,rdf)
 } yield ViolationError(errorType,focusNode,path,severity,scc,sourceShape,value)

 def noType(n: RDFNode, rdf: RDFReader): Boolean = {
   val types = objectsFromPredicate(rdf_type)(n,rdf)
   types.getOrElse(Set()).isEmpty
 }


 // TODO
 def includes(derefIncludes: Boolean): RDFParser[List[(IRI,Option[Manifest])]] = { (n,rdf) =>
   parseOk(List())
 }

 def parsePropertyValue[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = (n,rdf) => for {
   value <- objectFromPredicate(pred)(n,rdf)
   result <- parser(value,rdf)
 } yield result

 def parsePropertyValues[A](pred: IRI, parser: RDFParser[A]): RDFParser[Set[A]] = (n,rdf) => for {
   values <- objectsFromPredicate(pred)(n,rdf)
   results <- parseNodes(values.toList,parser)(rdf)
 } yield results.toSet

 def parsePropertyList[A](
   pred: IRI,
   parser: RDFParser[A]): RDFParser[List[A]] = (n,rdf) => for {
   ls <- rdfListForPredicate(pred)(n,rdf)
   vs <- parseNodes(ls,parser)(rdf)
 } yield vs

 /*
 def parseList[A](xs: List[RDFNode],
                  parser: RDFParser[A],
                  rdf: RDFReader): Try[List[A]] = {
   Try(xs.map(n => parser(n,rdf)).map(_.get))
 }

 def parseSet[A](xs: Set[RDFNode],
                 parser: RDFParser[A],
                 rdf: RDFReader): Try[Set[A]] = {
   Try(xs.map(n => parser(n,rdf)).map(_.get))
 }
*/

   def mapOptional[A,B](optA: Option[A], fn: A => Either[String,B]): Either[String, Option[B]] = {
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
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => parseFail(s"objectFromPredicate: Not found triples with subject $n and predicate $p \nRDF: ${rdf.serialize("TURTLE")}")
      case 1 => parseOk(ts.head.obj)
      case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
  }

  override def objectFromPredicateOptional(p:IRI): RDFParser[Option[RDFNode]] = { (n,rdf) => {
    optional(objectFromPredicate(p))(n,rdf)
   }
  }

  def iriFromPredicateOptional(p:IRI): RDFParser[Option[IRI]] = { (n,rdf) => {
    optional(iriFromPredicate(p))(n,rdf)
   }
  }

}

object RDF2Manifest extends RDF2Manifest {

 def read(fileName: String, format: String, base: Option[String]): Try[Manifest] = {
   for {
     cs <- {
       println(s"RDF2Manifest...$fileName")
       getContents(fileName)
     }
     rdf <- {
       println(s"Contents: ${cs.length} chars, format: $format, base: $base")
       RDFAsJenaModel.fromChars(cs, format, base)
     }
     mfs <- {
       println(s"Contents: ${cs.length} chars, format: $format, base: $base, rdf: ${rdf.model.size} triples")
       eitherToTry(rdf2Manifest(rdf, false))
     }
     if mfs.size == 1
   } yield mfs.head
 }

  def eitherToTry[A](e: Either[String,A]): Try[A] =  e match {
    case Left(msg) => Failure(new Exception(msg))
    case Right(ms) => Success(ms)
  }

}
