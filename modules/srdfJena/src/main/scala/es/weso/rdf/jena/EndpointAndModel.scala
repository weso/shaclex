package es.weso.rdf.jena
import java.io.ByteArrayOutputStream
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.jena.SPARQLQueries._
import es.weso.rdf.nodes.{RDFNode, _}
import es.weso.rdf.path.SHACLPath
import es.weso.rdf.triples.RDFTriple
import es.weso.utils.EitherUtils
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.query._
import org.apache.jena.rdf.model.{Model, Property, Statement, RDFNode => JenaRDFNode}
import org.slf4j._
import scala.jdk.CollectionConverters._
import scala.util.{Either, Left, Right, Try}
import cats._
import cats.implicits._

case class Compound(endpoints: List[Endpoint],
                    rdfModels: List[RDFAsJenaModel])
  extends RDFReader
     with RDFReasoner
     with LazyLogging {

  type Rdf = Compound

  val id = s"Compound"

  def availableParseFormats: List[String] = List()
  def availableSerializeFormats: List[String] = List()

  override def getPrefixMap: PrefixMap = {
    // TODO: Can we get more info about prefix maps from an endpoint?
    PrefixMap(Map())
  }

  val log = LoggerFactory.getLogger("Endpoint")

  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[IRI]): Either[String, Compound] = {
    throw new Exception("Cannot parse into a compound")
  }

  override def serialize(format: String, base: Option[IRI]): Either[String,String] = {
    Left(s"Endpoint cannot be serialized to $format")
  }

  override def iris(): Either[String,Set[IRI]] = ???
  override def subjects(): Either[String,Set[RDFNode]] = ???
  override def predicates(): Either[String, Set[IRI]] = ???
  override def iriObjects(): Either[String,Set[IRI]] = ???
  override def getSHACLInstances(c: RDFNode): Either[String,Seq[RDFNode]] = ???

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Either[String, Boolean] = ???

  override def nodesWithPath(path: SHACLPath): Either[String, Set[(RDFNode, RDFNode)]] = ???

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): Either[String, Set[RDFNode]] = ???

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): Either[String,Set[RDFNode]] = ???

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  override def rdfTriples(): Either[String,Set[RDFTriple]] = ???

  def triplesWithSubject(node: RDFNode): Either[String,Set[RDFTriple]] = ???

  def triplesWithPredicate(p: IRI): Either[String,Set[RDFTriple]] = {
    ???
  }

  def triplesWithObject(node: RDFNode): Either[String,Set[RDFTriple]] = {
    val xs = for {
      ts1 <- endpoints.map(_.triplesWithObject(node))
      ts2 <- rdfModels.map(_.triplesWithObject(node))
    } yield combine(ts1,ts2)
    flat(xs)
  }

  private def flat[A](ls: List[Either[String,Set[A]]]): Either[String,Set[A]] = {
    ls.sequence.map(_.flatten).map(_.toSet)
  }

  private def combine[A](es1: Either[String,Set[A]], es2: Either[String,Set[A]]): Either[String,Set[A]]=
    for {
      s1 <- es1
      s2 <- es2
    } yield s1 ++ s2

  def triplesWithPredicateObject(p: IRI, o: RDFNode): Either[String, Set[RDFTriple]] = {
    val xs = for {
      ts1 <- endpoints.map(_.triplesWithPredicateObject(p, o))
      ts2 <- rdfModels.map(_.triplesWithPredicateObject(p, o))
    } yield for {
      s1 <- ts1
      s2 <- ts2
    } yield s1 ++ s2
    xs.toList.sequence.map(_.flatten).map(_.toSet)
  }

  override def applyInference(inference: String): Either[String, Rdf] = {
    inference.toUpperCase match {
      case "NONE" => Right(this)
      case other => Left(s"Unsupported inference $other for compound model")
    }
  }

  override def availableInferenceEngines: List[String] = List("NONE")

  override def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]] = ???

  override def queryAsJson(queryStr: String): Either[String, Json] = ???

  override def getNumberOfStatements(): Either[String,Int] = ???

  override def isIsomorphicWith(other: RDFReader): Either[String,Boolean] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def asRDFBuilder: Either[String,RDFBuilder] =
    Left(s"Unimplemented isIsomorphicWith between endpoints")

  override def rdfReaderName: String = s"Compound"

  override def sourceIRI: Option[IRI] = None
}
