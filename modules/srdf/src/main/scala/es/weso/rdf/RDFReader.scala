package es.weso.rdf

import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import cats._
import cats.data._
import cats.implicits._

/**
 * RDFReader can get read RDF data from several sources
 * At this moment, it can parse RDF from <code>CharSequence</code>
 */
trait RDFReader {

  type Rdf <: RDFReader
  type ES[A] = Either[String,A]  // This is only needed to keep IntelliJ happy
  val id: String


  def availableParseFormats: List[String]

  def availableSerializeFormats: List[String]

  /**
    * Parse a char sequence to obtain an RDFReader
    * @param cs char sequence to parse
    * @param format format (TURTLE by default)
    * @param base base IRI (None by default)
    * @return Right RDF or Left error message
    */
  def fromString(cs: CharSequence,
                 format: String = "TURTLE",
                 base: Option[IRI] = None): Either[String,Rdf]

  /**
   * convert a RDF graph to a String
   */
  def serialize(format: String = "TURTLE", base: Option[IRI] = None): Either[String,String]

  /**
   * Set of RDFTriples in a graph
   */
  def rdfTriples(): Either[String,Set[RDFTriple]]

  /**
   * Returns the set of subjects that are IRIs in a graph
   */
  def subjects(): Either[String, Set[RDFNode]] = {
    rdfTriples.map(_.map(_.subj))
  }

  /**
   * Returns the set of predicates
   */
  def predicates(): Either[String, Set[IRI]] = {
    rdfTriples.map(_.map(_.pred))
  }

  /**
   * Returns the set of iriObjects that are IRIs in a graph
   */
  // TODO: Extend this to return all iriObjects: Seq[RDFNode]
  def iriObjects(): Either[String, Set[IRI]] = {
    rdfTriples.map(_.map(_.obj).collect { case i: IRI => i })
  }

  /**
   * The set of all iri's available
   */
  def iris(): Either[String, Set[IRI]] = {
    rdfTriples.map(_.map(_.iris).flatten)
  }

  /**
   * Set of RDFTriples that contain a node as subject
   * @param n node
   * @return A set of triples or a String with an error message
   */
  def triplesWithSubject(n: RDFNode): Either[String, Set[RDFTriple]]

  /**
   * Set of RDFTriples that relate two nodes by a predicate
   * @param p predicate
   */
  def triplesWithPredicate(p: IRI): Either[String, Set[RDFTriple]]

  /**
   * Set of RDFTriples that contain a node as object
   * @param n node
   */
  def triplesWithObject(n: RDFNode): Either[String, Set[RDFTriple]]


  /**
   * Set of RDFTriples that contain a node as predicate with some object
   * @param p predicate
   * @param o object
   */
  def triplesWithPredicateObject(p: IRI, o: RDFNode): Either[String, Set[RDFTriple]]

  /**
   * Set of RDFTriples that relate two nodes by a SHACL path
   * @param p path
   */
  def nodesWithPath(p: SHACLPath): Either[String, Set[(RDFNode, RDFNode)]]

  /**
   * Set of RDFTriples that relate a node with some object by a path
   * @param p path
   * @param o object
   */
  def subjectsWithPath(p: SHACLPath, o: RDFNode): Either[String, Set[RDFNode]]

  /**
   * return the values associated with a node by a path
   * The path is defined as in SHACL paths which are a simplified version of SPARQL paths
   */
  def objectsWithPath(subj: RDFNode, path: SHACLPath): Either[String, Set[RDFNode]]

  def triplesWithType(expectedType: IRI): Either[String, Set[RDFTriple]] = {
    triplesWithPredicateObject(`rdf:type`, expectedType)
  }

  /**
   * Set of RDFTriples that contain a node as subject and a given Predicate
   * @param s
   */
  def triplesWithSubjectPredicate(s: RDFNode, p: IRI): Either[String, Set[RDFTriple]] = for {
    // This is the default implementation which is not optimized
    // Implementations of RDFReader could override this implementation by a more efficient one
    ts <- triplesWithSubject(s)
  } yield ts.filter(_.hasPredicate(p))

  def hasPredicateWithSubject(n: RDFNode, p: IRI): Either[String, Boolean] = {
    triplesWithSubjectPredicate(n, p)map(_.size > 0)
  }

  /**
    * Set of RDFTriples that contain a node as object with some of the predicates in a list
    * @param o object
    * @param ps list of predicates
    */
  def triplesWithPredicatesObject(ps: List[IRI], o: RDFNode): Either[String, Set[RDFTriple]] = {
    ps.map(triplesWithPredicateObject(_,o)).
    sequence[ES,Set[RDFTriple]].
    map(_.flatten.toSet)
  }

  /**
    * Set of RDFTriples that contain a node as subject with some of the predicates in a list
    * @param n node
    * @param ps list of predicates
    */
  def triplesWithSubjectPredicates(n: RDFNode, ps: List[IRI]): Either[String, Set[RDFTriple]] = {
    ps.map(p => triplesWithSubjectPredicate(n,p)).sequence[ES,Set[RDFTriple]].map(_.flatten.toSet)
  }

  /**
   * Prefix map
   */
  def getPrefixMap(): PrefixMap

  /**
   * `true` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def hasSHACLClass(node: RDFNode, cls: RDFNode): Either[String, Boolean]

  /**
   * return the SHACL instances of a node `cls`
   * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
   */
  def getSHACLInstances(cls: RDFNode): Either[String, Seq[RDFNode]]

  def getTypes(node: RDFNode): Either[String,Set[RDFNode]] = {
    triplesWithSubjectPredicate(node, `rdf:type`).map(_.map(_.obj))
  }

  /**
    * Checks if a node has a given datatype
    * @param node RDF node to check
    * @param datatype Datatype IRI to check
    * @return In case of a bad formed literal, a Left with a message, otherwise the check
    */
  def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean]

  /**
    * Run a SPARQL query which returns a JSON representation of the result
    * @param str string representing the SPARQL query
    * @return JSON representation of the result
    */
  def queryAsJson(str: String): Either[String,Json]

  /**
    * Run a SPARQL select query which returns a result map
    * @param queryStr string representing the SPARQL query
    * @return Either a List of mappings or an error message
    */
  def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]]

  def getNumberOfStatements(): Either[String,Int]

  /**
  *
    * @param other RDF reader
    * @return true if this RDF graph is isomorphic with other
    */
  def isIsomorphicWith(other: RDFReader): Either[String,Boolean]

  /**
    * @return Source IRI of this RDF graph if exists
    */
  def sourceIRI: Option[IRI]

  def asRDFBuilder: Either[String, RDFBuilder]

  def rdfReaderName: String

  def subjectsWithType(t: RDFNode): Either[String, Set[RDFNode]] = {
    triplesWithPredicateObject(`rdf:type`, t).map(_.map(_.subj))
  }

  def subjectsWithProperty(pred: IRI): Either[String, Set[RDFNode]] = {
    triplesWithPredicate(pred).map(_.map(_.subj))
  }

}

