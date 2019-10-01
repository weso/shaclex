package es.weso.rdf.jena
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.nodes.{RDFNode, _}
import es.weso.rdf.path.SHACLPath
import es.weso.rdf.triples.RDFTriple
import io.circe.Json
import org.slf4j._
import scala.util._
import cats.implicits._

case class Compound(members: List[RDFReader])
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

  private def cmb[A, B, C](ls: List[A],
                           f: A => Either[String,Set[C]]
                          ): Either[String,Set[C]] = {
    val vs = ls.map(f(_))
    vs.sequence.map(_.toSet).map(_.flatten)
  }

  private def cmbSeq[A, B, C](ls: List[A],
                           f: A => Either[String,Seq[C]]): Either[String,Seq[C]] = {
    val vs = ls.map(f(_))
    vs.sequence.map(_.toSeq).map(_.flatten)
  }

  override def iris(): Either[String,Set[IRI]] =
    cmb(members, (e:RDFReader) => e.iris())

  override def subjects(): Either[String,Set[RDFNode]] =
    cmb(members, (e:RDFReader) => e.subjects())

  override def predicates(): Either[String, Set[IRI]] =
    cmb(members, (e:RDFReader) => e.predicates())

  override def iriObjects(): Either[String,Set[IRI]] =
    cmb(members, (e:RDFReader) => e.iriObjects())

  override def getSHACLInstances(c: RDFNode): Either[String,Seq[RDFNode]] =
    cmbSeq(members,
      (e:RDFReader) => e.getSHACLInstances(c))

  private def someTrue(xs: Set[Boolean]): Boolean = xs.exists(_ == true)

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Either[String, Boolean] = {
    val vs = members.map(_.hasSHACLClass(n,c))
    vs.sequence.map(_.toSet).map(someTrue(_))
  }

  override def nodesWithPath(path: SHACLPath): Either[String, Set[(RDFNode, RDFNode)]] =
    cmb(members, (e:RDFReader) => e.nodesWithPath(path))


  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): Either[String, Set[RDFNode]] =
    cmb(members, (e:RDFReader) => e.subjectsWithPath(path,obj))

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): Either[String,Set[RDFNode]] =
    cmb(members, (e:RDFReader) => e.objectsWithPath(subj,path))

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  override def rdfTriples(): Either[String,Set[RDFTriple]] =
    cmb(members, (e:RDFReader) => e.rdfTriples())


  def triplesWithSubject(node: RDFNode): Either[String,Set[RDFTriple]] =
    cmb(members, (e:RDFReader) => e.triplesWithSubject(node))

  def triplesWithPredicate(p: IRI): Either[String,Set[RDFTriple]] = {
    cmb(members, (e:RDFReader) => e.triplesWithPredicate(p))
  }

  def triplesWithObject(node: RDFNode): Either[String,Set[RDFTriple]] = {
    cmb(members, (e:RDFReader) => e.triplesWithObject(node))
  }

  def triplesWithPredicateObject(p: IRI, o: RDFNode): Either[String, Set[RDFTriple]] = {
    cmb(members, (e:RDFReader) => e.triplesWithPredicateObject(p,o))
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
