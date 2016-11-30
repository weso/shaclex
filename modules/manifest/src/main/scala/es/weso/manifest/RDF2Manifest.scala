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
     ): Try[Seq[Manifest]] = {
    val candidates = subjectsWithType(mf_Manifest,rdf).toSeq
    val maybeManifests = candidates.map {
      case node => manifest(derefIncludes)(node,rdf)
    }
    for {
      manifests <- filterSuccess(maybeManifests)
    } yield manifests
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

 def getEntryType(node: RDFNode): Try[EntryType] = {
   node match {
     case `sht_Validate` => Success(Validate)
     case `sht_ValidationTest` => Success(ValidationTest)
     case `sht_ValidationFailure` => Success(ValidationFailure)
     case `sht_MatchNodeShape` => Success(MatchNodeShape)
     case `sht_WellFormedSchema` => Success(WellFormedSchema)
     case `sht_NonWellFormedSchema` => Success(NonWellFormedSchema)
     case `sht_ConvertSchemaSyntax` => Success(ConvertSchemaSyntax)
     case _ => Failure(RDF2ManifestException("Unexpected entry type: " + node))
   }
 }

 def entry:RDFParser[Entry] = { (n,rdf) =>
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
     Entry(entryType = entryType,
      name = name,
      action = action,
      result = result,
      status = Status(statusIri),
      specRef = specRef
     )
 }


 def iriDataFormat2str(iri: IRI): Try[String] = {
   iri match {
     case `sht_TURTLE` => Success("TURTLE")
     case _ => fail("Unexpected schema format: " + iri)
   }
 }

 def iriSchemaFormat2str(iri: IRI): Try[String] = {
   iri match {
     case `sht_SHACLC` => Success("SHACLC")
     case `sht_TURTLE` => Success("TURTLE")
     case _ => fail("Unexpected schema format: " + iri)
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
     case BooleanLiteral(b) => Success(BooleanResult(b))
     case iri:IRI =>
       if (noType(iri,rdf)) Success(IRIResult(iri))
       else compoundResult(iri,rdf)
     case bNode: BNodeId => compoundResult(bNode,rdf)
     case _ => fail("Unexpected type of result " + n)
   }
  }
 }

 def compoundResult: RDFParser[Result] = (n,rdf) => {
  iriFromPredicate(rdf_type)(n,rdf) match {
    case Success(iri) => iri match {
      case `sht_NotValid` => for {
        detailsNode <- objectFromPredicate(sht_details)(n,rdf)
        validationReport <- parsePropertyValue(sht_validationReport,validationReport)(detailsNode, rdf)
        validatedPairs <- parsePropertyValue(sht_validatedPairs, validatedPairs)(detailsNode, rdf)
        } yield NotValidResult(validationReport, validatedPairs)
      case `sht_Valid` => Success(ValidResult(List()))
      case _ => fail(s"Unsupporte type of compound result: $iri")
    }
    case Failure(e) => fail(s"Couldn't obtain rdf:type of node: $n")
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
   Success(List())
 }

   def parsePropertyValue[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = (n,rdf) => for {
   value <- objectFromPredicate(pred)(n,rdf)
   result <- parser(value,rdf)
 } yield result

 def parsePropertyValues[A](pred: IRI, parser: RDFParser[A]): RDFParser[Set[A]] = (n,rdf) => for {
   values <- objectsFromPredicate(pred)(n,rdf)
   results <- parseSet(values,parser,rdf)
 } yield results

 def parsePropertyList[A](
   pred: IRI,
   parser: RDFParser[A]): RDFParser[List[A]] = (n,rdf) => for {
   ls <- rdfListForPredicate(pred)(n,rdf)
   vs <- parseList(ls,parser,rdf)
 } yield vs

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

   def mapOptional[A,B](optA: Option[A], fn: A => Try[B]): Try[Option[B]] = {
   optA match {
     case None => Success(None)
     case Some(x) => {
       fn(x) match {
         case Success(v) => Success(Some(v))
         case Failure(e) => Failure(e)
       }
     }
   }
 }

 def oneOfPredicates(predicates: Seq[IRI]): RDFParser[IRI] = {
   val ps = predicates.map(iriFromPredicate(_))
   oneOf(ps)
 }

 def fail[A](str: String): Try[A] =
   Failure(new Exception(str))

 /**
  * Override this method to provide more info
  */
 override def objectFromPredicate(p: IRI): RDFParser[RDFNode] = { (n, rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => fail(s"objectFromPredicate: Not found triples with subject $n and predicate $p \nRDF: ${rdf.serialize("TURTLE")}")
      case 1 => Success(ts.head.obj)
      case _ => fail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
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

  override def optional[A](parser:RDFParser[A]): RDFParser[Option[A]] = { (n,rdf) => {
    parser(n,rdf) match {
      case Success(v) => Success(Some(v))
      case Failure(e) => {
        Success(None)
      }
    }
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
       println(s"Contents: $cs, format: $format, base: $base")
       RDFAsJenaModel.fromChars(cs, format, base)
     }
     mfs <- {
       println(s"Contents: $cs, format: $format, base: $base, rdf: $rdf")
       rdf2Manifest(rdf, false)
     }
     if mfs.size == 1
   } yield mfs.head
 }

}
