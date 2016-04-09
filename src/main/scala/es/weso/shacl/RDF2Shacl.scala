package es.weso.shacl

import scala.util.{ Failure, Success, Try }

import org.slf4s.Logging

import es.weso.rdf.{ PrefixMap, RDFReader }
import es.weso.rdf.nodes.{ BNodeId, IRI, Literal, RDFNode }
import es.weso.rdf.parser.RDFParser
import es.weso.utils.TryUtils._

import SHACLPrefixes._

object RDF2Shacl 
    extends Logging
    with RDFParser {
  
  /**
   * Parses RDF content and obtains a SHACL Schema and a PrefixMap 
   */
  def getShacl(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap  
    for {
      shapes <- shapes(rdf)
    } yield (Schema(shapes), pm)
  }
  
  def shapes(rdf: RDFReader): Try[Seq[Shape]] = {
   val shape_nodes = subjectsWithType(sh_Shape, rdf)
   filterSuccess(shape_nodes.toSeq.map (node => shape(node,rdf)))
  }
  
  def shape(node: RDFNode, rdf: RDFReader): Try[Shape] = {
    node match {
      case iri : IRI => {
        Success(Shape(Some(iri),noScopes, noFilters, noConstraints))
      }
      case _ => fail("Non supported shapes without IRI Id")
    }
  }
  
  def fail(str: String) = 
    Failure(throw new Exception(str))
    
  def noScopes: Seq[Scope] = Seq()
  def noFilters: Seq[Shape] = Seq()
  def noConstraints: Seq[Constraint] = Seq()

}
