package es.weso.rdf.jena

import java.io.ByteArrayOutputStream

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple

import scala.collection.JavaConverters._
import scala.util.{Either, Left, Right, Try}
import org.apache.jena.rdf.model.Property
import org.apache.jena.rdf.model.Statement
import org.apache.jena.rdf.model.Model
import org.slf4j._
import es.weso.rdf._
import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.path.SHACLPath
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.jena.JenaMapper._

// TODO: Refactor to change String type by IRI
case class Endpoint(endpointIRI: IRI)
  extends RDFReader
     with RDFReasoner
     with LazyLogging {
  type Rdf = Endpoint

  val endpoint = endpointIRI.str
  val id = s"Endpoint($endpoint)"

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: PrefixMap = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    PrefixMap(Map())
  }

  val log = LoggerFactory.getLogger("Endpoint")

  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[IRI]): Either[String, Endpoint] = {
    throw new Exception("Cannot parse into an endpoint. endpoint = " + endpoint)
  }

  override def serialize(format: String, base: Option[IRI]): Either[String,String] = {
    Left(s"Endpoint with url $endpoint. Cannot be serialized to $format")
  }

  override def iris(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.asScala.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
  }

  override def subjects(): Set[RDFNode] = {
    // TODO: The following code only returns resource IRIs (no BNodes)
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.asScala.map(qs => IRI(qs.get("x").asResource.getURI)).toSet
  }

  override def predicates(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findPredicates).execSelect()
    resultSet.asScala.map(qs => IRI(qs.get("p").asResource.getURI)).toSet
  }

  override def iriObjects(): Set[IRI] = {
    val resultSet = QueryExecutionFactory.sparqlService(endpoint, findIRIs).execSelect()
    resultSet.asScala.map(qs => IRI(qs.get("y").asResource.getURI)).toSet
  }

  override def getSHACLInstances(c: RDFNode): Either[String,Seq[RDFNode]] = {
    Left(s"getSHACLInstances: Not implemented at Endpoint, node $c")
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Either[String, Boolean] = {
    Left(s"hasSHACLClass: Not implemented at Endpoint. Node: $n Class: $c")
  }

  override def nodesWithPath(p: SHACLPath): Either[String, Set[(RDFNode, RDFNode)]] = {
    Left(s"Undefined nodesWithPath at RDFFromWeb. Path: $p")
  }

  override def subjectsWithPath(p: SHACLPath, o: RDFNode): Either[String, Set[RDFNode]] = {
    Left(s"Undefined subjectsWithPath at RDFFromWeb. Path: $p")
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): Either[String,Set[RDFNode]] = {
    Left(s"Undefined objectsWithPath at RDFFromWeb. Path: $path")
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)


  def rdfTriples(): Set[RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriples).execConstruct()
    model2triples(model)
  }

  def triplesWithSubject(node: RDFNode): Either[String,Set[RDFTriple]] = node match {
    case subj: IRI => Try {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithSubject(subj)).execConstruct()
      model2triples(model)
    }.fold(e => Left(s"Error accessing endpoint ${endpoint} to obtain triples with subject $node: ${e.getMessage}"), Right(_))
    case _ => Right(Set()) // Left("triplesWithSubject: node " + node + " must be a IRI")
  }

  def triplesWithPredicate(p: IRI): Either[String,Set[RDFTriple]] = {
    val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicate(p)).execConstruct()
    Right(model2triples(model))
  }

  def triplesWithObject(node: RDFNode): Either[String,Set[RDFTriple]] = node match {
    case obj: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithObject(obj)).execConstruct()
      Right(model2triples(model))
    }
    case _ => Left("triplesWithObject: node " + node + " must be a IRI")
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): Either[String, Set[RDFTriple]] =
    o match {
    case iri: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint, queryTriplesWithPredicateObject(p, iri)).execConstruct()
      Right(model2triples(model))
    }
    case _ => Left("triplesWithPredicateObject: o " + o + " must be a IRI")
  }

  def model2triples(model: Model): Set[RDFTriple] = {
    val ts = model.listStatements().asScala.map(st => statement2triple(st)).toSet
    logger.debug(s"Total triples = ${ts.size}")
    ts
  }

  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      jena2rdfnode(st.getSubject),
      property2iri(st.getPredicate),
      jena2rdfnode(st.getObject))
  }

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  def jena2rdfnode(r: JenaRDFNode): RDFNode = {
    if (r.isAnon) {
      BNode(r.asNode.getBlankNodeId.getLabelString)
    } else if (r.isURIResource) {
      IRI(r.asResource().getURI())
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

  override def applyInference(inference: String): Either[String, Rdf] = {
    inference.toUpperCase match {
      case "NONE" => Right(this)
      case other => Left(s"Unsupported inference $other for endpoint $endpoint")
    }
  }

  override def availableInferenceEngines: List[String] = List("NONE")

  override def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]] = {
    val tryQuery: Try[List[Map[String,RDFNode]]] = Try {
      val query = QueryFactory.create(queryStr)
      val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result = qExec.execSelect()
          // val varNames = result.getResultVars
          val ls: List[Map[String, RDFNode]] = result.asScala.toList.map(qs => {
            val qsm = new QuerySolutionMap()
            qsm.addAll(qs)
            qsm.asMap.asScala.toMap.mapValues(node => jenaNode2RDFNodeUnsafe(node))
          })
          ls
        }
        case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
      }
    }
    tryQuery.toEither.leftMap(_.getMessage)
  }

  override def queryAsJson(queryStr: String): Either[String, Json] = Try {
    val query = QueryFactory.create(queryStr)
    val qExec = QueryExecutionFactory.sparqlService(endpoint, query)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()
        val outputStream = new ByteArrayOutputStream()
        ResultSetFormatter.outputAsJSON(outputStream, result)
        val jsonStr = new String(outputStream.toByteArray())
        parse(jsonStr).leftMap(f => f.getMessage)
      }
      case Query.QueryTypeConstruct => {
        // val result = qExec.execConstruct()
        Left(s"Unimplemented CONSTRUCT queries yet")
      }
      case Query.QueryTypeAsk => {
        val result = qExec.execAsk()
        Right(Json.fromBoolean(result))
      }
      case Query.QueryTypeDescribe => {
        Left(s"Unimplemented DESCRIBE queries yet")
      }
      case _ => {
        Left(s"Unknown type of query. Not implemented")
      }
    }
  }.toEither.fold(f => Left(f.getMessage), es => es)

  override def getNumberOfStatements(): Either[String,Int] = {
    Try{
      val resultSet = QueryExecutionFactory.sparqlService(endpoint, countStatements).execSelect()
      resultSet.asScala.map(qs => qs.get("c").asLiteral().getInt).toList.head
    }.toEither.leftMap(_.getMessage)
  }

  override def isIsomorphicWith(other: RDFReader): Either[String,Boolean] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def sourceIRI = None

  override def asRDFBuilder: Either[String,RDFBuilder] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def rdfReaderName: String = s"Endpoint($endpoint)"

}

object Endpoint {
  def fromString(str: String): Either[String,Endpoint] = {
    IRI.fromString(str).map(Endpoint(_))
  }
}