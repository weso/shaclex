package es.weso.rdf

import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import scala.util.Try

/**
 * RDFReader can get read RDF data from several sources
 * At this moment, it can parse RDF from <code>CharSequence</code>
 */
trait RDFReader {

  type Rdf <: RDFReader

  /**
   * parse a string and obtain an RDF graph
   */
  def parse(cs: CharSequence, format: String = "TURTLE", base: Option[String] = None): Try[Rdf]

  /**
   * convert a RDF graph to a String
   */
  def serialize(format: String = "TURTLE"): String

  /**
   * Set of RDFTriples in a graph
   */
  def rdfTriples(): Set[RDFTriple]

  /**
   * Returns the set of subjects that are IRIs in a graph
   */
  def subjects(): Set[RDFNode] = {
    rdfTriples.map(_.subj)
  }

  /**
   * Returns the set of predicates
   */
  def predicates(): Set[IRI] = {
    rdfTriples.map(_.pred)
  }

  /**
   * Returns the set of objects that are IRIs in a graph
   */
  // TODO: Extend this to return all objects: Seq[RDFNode]
  def objects(): Set[IRI] = {
    rdfTriples.map(_.obj).filter(_.isIRI).map(_.toIRI)
  }

  /**
   * The set of all iri's available
   */
  def iris(): Set[IRI] = {
    rdfTriples.map(_.iris).flatten
  }

  /**
   * Set of RDFTriples that contain a node as subject
   * @param n node
   */
  def triplesWithSubject(n: RDFNode): Set[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as predicate
   * @param p predicate
   */
  def triplesWithPredicate(p: IRI): Set[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as object
   * @param n node
   */
  def triplesWithObject(n: RDFNode): Set[RDFTriple]

  /**
   * Set of RDFTriples that contain a node as predicate with some object
   * @param p predicate
   * @param o object
   */
  def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple]

  def triplesWithType(expectedType: IRI): Set[RDFTriple] = {
    triplesWithPredicateObject(rdf_type, expectedType)
  }

  /**
   * Set of RDFTriples that contain a node as subject and a given Predicate
   * @param s
   */
  def triplesWithSubjectPredicate(s: RDFNode, p: IRI): Set[RDFTriple] = {
    triplesWithSubject(s).filter(t => t.hasPredicate(p))
  }

  def hasPredicateWithSubject(n: RDFNode, p: IRI): Boolean = {
    triplesWithSubjectPredicate(n, p).size > 0
  }
  
  /**
   * Prefix map
   */
  def getPrefixMap(): PrefixMap

  /**
   * `true` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def hasSHACLClass(node: RDFNode, cls: RDFNode): Boolean
  
  /**
   * return the SHACL instances of a node `cls`
   * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def getSHACLInstances(cls: RDFNode): Seq[RDFNode]

}

