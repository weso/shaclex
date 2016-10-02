package es.weso.manifest

import es.weso.rdf.nodes._
import es.weso.rdf._
import scala.util._
import es.weso.rdf._
import ManifestPrefixes._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.PrefixMap
import es.weso.utils.IO._
import es.weso.rdf.parser.RDFParser
import es.weso.utils.TryUtils._
import es.weso.utils.ConsoleDebugger
import es.weso.rdf.jena.RDFAsJenaModel
import java.io.File


case class RDF2ManifestException(msg:String)
 extends Exception(msg)

trait RDF2Manifest
  extends RDFParser {

  def rdf2Manifest(
     rdf: RDFReader,
     derefIncludes: Boolean
     ): Try[Seq[Manifest]] = {
    ConsoleDebugger.debugStep("RDF2Manifest")
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

 def entries:RDFParser[Seq[Entry]] = { (n,rdf) => {
   for {
     es <- rdfListForPredicate(mf_entries)(n,rdf)
     entries <- group(entry,es)(n,rdf)
   } yield entries
 }
 }

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
     resultNodes <- objectsFromPredicate(mf_result)(n,rdf)
     results <- parseList(resultNodes,result,rdf)
     statusIri <- iriFromPredicate(mf_status)(n,rdf)
     specRef <- optional(iriFromPredicate(sht_specRef))(n,rdf)
   } yield
    Entry(
        entryType = entryType,
        name = name,
        action = action,
        results = results,
        status = Status(statusIri),
        specRef = specRef
    )
 }

 def parseList[A](xs: Set[RDFNode], p: RDFParser[A], rdf: RDFReader): Try[Set[A]] = {
   Try(xs.map(n => p(n,rdf)).map(_.get))
 }

 def fail[A](str: String): Try[A] =
   Failure(throw new Exception(str))

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
     schema <- optional(iriFromPredicate(sht_schema))(n,rdf)
     schemaFormatIRI <- optional(iriFromPredicate(sht_schema_format))(n,rdf)
     schemaFormat <- mapOptional(schemaFormatIRI,iriSchemaFormat2str)
     data <- optional(iriFromPredicate(sht_data))(n,rdf)
     dataFormatIri <- optional(iriFromPredicate(sht_data_format))(n,rdf)
     dataFormat <- mapOptional(dataFormatIri,iriDataFormat2str)
     schemaOutputFormat <- optional(iriFromPredicate(sht_schema_output_format))(n,rdf)
     node <- optional(oneOfPredicates(Seq(sht_node,sht_focus)))(n,rdf)
     shape <- optional(iriFromPredicate(sht_shape))(n,rdf)
   } yield
       ManifestAction(
           schema = schema,
           schemaFormat = schemaFormat,
           data = data,
           dataFormat = dataFormat,
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
   println(s"Parsing result...$n")
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

 def compoundResult: RDFParser[Result] = ???

 def noType(n: RDFNode, rdf: RDFReader): Boolean = ???

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

 // TODO
 def includes(derefIncludes: Boolean):
     RDFParser[List[(IRI,Option[Manifest])]] = { (n,rdf) =>
   Success(List())
 }

}

object RDF2Manifest extends RDF2Manifest {

 def read(fileName: String, base: String): Try[Manifest] = {
   for {
     cs <- getContents(fileName)
     rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE", Some(base))
     mfs <- rdf2Manifest(rdf, false)
     if mfs.size == 1
   } yield mfs.head
 }

}
