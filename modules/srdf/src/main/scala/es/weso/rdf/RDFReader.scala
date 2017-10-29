package es.weso.rdf

import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path.SHACLPath

import scala.util.Try

/**
 * RDFReader can get read RDF data from several sources
 * At this moment, it can parse RDF from <code>CharSequence</code>
 */
trait RDFReader {

  type Rdf <: RDFReader

  /**
    * Parse a char sequence to obtain an RDFReader
    * @param cs char sequence to parse
    * @param format format (TURTLE by default)
    * @param base base IRI (None by default)
    * @return Right RDF or Left error message
    */
  def parse(cs: CharSequence,
            format: String = "TURTLE",
            base: Option[String] = None): Either[String,Rdf]

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
   * Returns the set of iriObjects that are IRIs in a graph
   */
  // TODO: Extend this to return all iriObjects: Seq[RDFNode]
  def iriObjects(): Set[IRI] = {
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
   * Set of RDFTriples that relate two nodes by a predicate
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

  /**
   * Set of RDFTriples that relate two nodes by a SHACL path
   * @param p path
   */
  def nodesWithPath(p: SHACLPath): Set[(RDFNode, RDFNode)]

  /**
   * Set of RDFTriples that relate a node with some object by a path
   * @param p path
   * @param o object
   */
  def subjectsWithPath(p: SHACLPath, o: RDFNode): Set[RDFNode]

  /**
   * return the values associated with a node by a path
   * The path is defined as in SHACL paths which are a simplified version of SPARQL paths
   */
  def objectsWithPath(subj: RDFNode, path: SHACLPath): Set[RDFNode]

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

  def getTypes(node: RDFNode): Set[RDFNode] = {
    triplesWithSubjectPredicate(node, rdf_type).map(_.obj)
  }

  /**
    * Checks if a node has a given datatype
    * @param node RDF node to check
    * @param datatype Datatype IRI to check
    * @return In case of a bad formed literal, a Left with a message, otherwise the check
    */
  def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean]

}

