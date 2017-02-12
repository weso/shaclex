package es.weso.rdf.jena

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple

import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdf.triples._
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import org.apache.jena.rdf.model.Property
import org.apache.jena.rdf.model.Statement
import org.apache.jena.rdf.model.Model
import org.slf4j._
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.path.SHACLPath
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}

case class RDFFromWeb() extends RDFReader {
  type Rdf = RDFFromWeb

  val log = LoggerFactory.getLogger("RDFFromWeb")

  override def getPrefixMap: PrefixMap = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    PrefixMap(Map())
  }

  override def parse(cs: CharSequence, format: String, base: Option[String]): Try[Rdf] = {
    throw new Exception("Cannot parse RDFFromWeb ")
  }

  override def serialize(format: String): String = {
    throw new Exception("Cannot serialize RDFFromWeb")
  }

  override def rdfTriples(): Set[RDFTriple] = {
    throw new Exception("Cannot obtain triples from RDFFromWeb ")
  }

  override def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val subj = node.toIRI
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, subj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithSubject(subj), derefModel).execConstruct()
      val triples = model2triples(model)
      log.debug("triples with subject " + subj + " =\n" + triples)
      triples
    } else
      throw new Exception("triplesWithSubject: node " + node + " must be a IRI")
  }

  override def triplesWithPredicate(p: IRI): Set[RDFTriple] = {
    val derefModel = ModelFactory.createDefaultModel
    RDFDataMgr.read(derefModel, p.str)
    val model = QueryExecutionFactory.create(queryTriplesWithPredicate(p), derefModel).execConstruct()
    model2triples(model)
  }

  override def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val obj = node.toIRI
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, obj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithObject(obj), derefModel).execConstruct()
      model2triples(model)
    } else
      throw new Exception("triplesWithObject: node " + node + " must be a IRI")
  }

  override def triplesWithPredicateObject(p: IRI, node: RDFNode): Set[RDFTriple] = {
    if (node.isIRI) {
      val obj = node.toIRI
      val derefModel = ModelFactory.createDefaultModel
      RDFDataMgr.read(derefModel, obj.str)
      val model = QueryExecutionFactory.create(queryTriplesWithPredicateObject(p, obj), derefModel).execConstruct()
      model2triples(model)
    } else
      throw new Exception("triplesWithObject: node " + node + " must be a IRI")
  }

  override def getSHACLInstances(c: RDFNode): Seq[RDFNode] = {
    throw new Exception(s"Undefined getSHACLInstances at RDFFromWeb. Node $c") 
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Boolean = {
    throw new Exception(s"Undefined hasSHACL at RDFFromWeb. Node: $n Class: $c") 
  }

  def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().map(st => statement2triple(st)).toSet
  }

  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      jena2rdfnode(st.getSubject),
      property2iri(st.getPredicate),
      jena2rdfnode(st.getObject)
    )
  }

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  def jena2rdfnode(r: JenaRDFNode): RDFNode = {
    if (r.isAnon) {
      BNodeId(r.asNode.getBlankNodeId.getLabelString)
    } else if (r.isURIResource) {
      IRI(r.asResource.getURI())
    } else if (r.isLiteral) {
      val lit = r.asLiteral
      if (lit.getDatatypeURI() == null) {
        StringLiteral(lit.getString())
      } else
        IRI(lit.getDatatypeURI()) match {
          case RDFNode.IntegerDatatypeIRI => IntegerLiteral(lit.getInt)
          case RDFNode.BooleanDatatypeIRI => BooleanLiteral(lit.getBoolean)
          case RDFNode.DoubleDatatypeIRI => DoubleLiteral(lit.getDouble())
          case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
          case _ => DatatypeLiteral(lit.getLexicalForm, IRI(lit.getDatatypeURI))
        }
    } else
      throw new Exception("Unknown type of resource")
  }

override def getValuesFromPath(node: RDFNode, path: SHACLPath) =
  throw new Exception(s"Undefined getValuesFromPath at RDFFromWeb. Node $node, path: $path")


}