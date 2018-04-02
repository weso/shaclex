package es.weso.rdf.jena

import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple

import scala.collection.JavaConverters._
import scala.util.Try
import es.weso.rdf._
import org.apache.jena.rdf.model.{Model, Property, Resource, Statement, RDFNode => JenaRDFNode}
import org.slf4j._
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.rdf.model.ModelFactory

import scala.util._
import java.io._

import org.apache.jena.riot.RDFLanguages._
import org.apache.jena.riot.RDFLanguages
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.path.SHACLPath
import es.weso.utils._
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.query.{Query, QueryExecutionFactory, QuerySolutionMap, ResultSetFormatter}
import org.apache.jena.sparql.path.Path
import cats.implicits._

case class RDFAsJenaModel(model: Model)
  extends RDFReader
  with RDFBuilder
  with RDFReasoner {

  type Rdf = RDFAsJenaModel

  val log = LoggerFactory.getLogger("RDFAsJenaModel")

  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[String] = None): Either[String, Rdf] = {
    Try {
      val m = ModelFactory.createDefaultModel
      val str_reader = new StringReader(cs.toString)
      val baseURI = base.getOrElse("")
      RDFDataMgr.read(m, str_reader, baseURI, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs"),
      Right(_)
    )
  }

  private def getRDFFormat(formatName: String): Either[String,String] = {
    val supportedFormats: List[String] = RDFLanguages.getRegisteredLanguages().asScala.toList.map(_.getName.toUpperCase).distinct
    formatName.toUpperCase match {
      case format if supportedFormats.contains(format) => Right(format)
      case unknown => Left(s"Unsupported format $unknown. Available formats: ${supportedFormats.mkString(",")} ")
    }
  }

  override def serialize(formatName: String): Either[String, String] = for {
    format <- getRDFFormat(formatName)
    str <- Try {
     val out: StringWriter = new StringWriter()
     model.write(out, format)
     out.toString
    }.fold(e => Left(s"Error serializing RDF to format $formatName: $e"),
      Right(_)
    )
  } yield str

/*  private def extend_rdfs: Rdf = {
    val infModel = ModelFactory.createRDFSModel(model)
    RDFAsJenaModel(infModel)
  } */

  // TODO: this implementation only returns subjects
  override def iris(): Set[IRI] = {
    val resources: Set[Resource] = model.listSubjects().asScala.toSet
    resources.filter(s => s.isURIResource).map(r => IRI(r.getURI))
  }

  override def subjects(): Set[RDFNode] = {
    val resources: Set[Resource] = model.listSubjects().asScala.toSet
    resources.map(r => jenaNode2RDFNode(r))
  }

  override def rdfTriples(): Set[RDFTriple] = {
    model2triples(model)
  }

  override def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    val maybeResource = rdfNode2Resource(node, model)
    val empty: Set[RDFTriple] = Set()
    maybeResource.fold(empty) {
      case resource =>
        val statements: Set[Statement] = triplesSubject(resource, model)
        toRDFTriples(statements)
    }
  }

  /**
    * return the SHACL instances of a node `cls`
    * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
    */
  override def getSHACLInstances(c: RDFNode): Seq[RDFNode] = {
    val cJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.getSHACLInstances(cJena, model).map(n => JenaMapper.jenaNode2RDFNode(n))
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Boolean = {
    val nJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(n, model)
    val cJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.hasClass(nJena, cJena, model)
  }

  override def nodesWithPath(path: SHACLPath): Set[(RDFNode, RDFNode)] = {
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val pairs = JenaUtils.getNodesFromPath(jenaPath, model).
      map(p => (JenaMapper.jenaNode2RDFNode(p._1), JenaMapper.jenaNode2RDFNode(p._2)))
    pairs.toSet
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): Set[RDFNode] = {
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(subj, model)
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val nodes = JenaUtils.objectsFromPath(jenaNode, jenaPath, model).map(n => JenaMapper.jenaNode2RDFNode(n))
    nodes.toSet
  }

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): Set[RDFNode] = {
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(obj, model)
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val nodes = JenaUtils.subjectsFromPath(jenaNode, jenaPath, model).map(n => JenaMapper.jenaNode2RDFNode(n))
    nodes.toSet
  }

  private def toRDFTriples(ls: Set[Statement]): Set[RDFTriple] = {
    ls.map(st => statement2triple(st))
  }

  override def triplesWithPredicate(node: IRI): Set[RDFTriple] = {
    val pred = rdfNode2Property(node, model)
    val statements: Set[Statement] = triplesPredicate(pred, model)
    toRDFTriples(statements)
  }

  override def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    val obj = rdfNode2Resource(node, model)
    val empty: Set[RDFTriple] = Set()
    obj.fold(empty) { o => {
      val statements: Set[Statement] = triplesObject(o, model)
      toRDFTriples(statements)
    }
    }
  }

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple] = {
    val pred = rdfNode2Property(p, model)
    val maybeObj = rdfNode2Resource(o, model)
    val empty: Set[RDFTriple] = Set()
    maybeObj.fold(empty) { obj =>
      val statements: Set[Statement] = triplesPredicateObject(pred, obj, model)
      toRDFTriples(statements)
    }
  }

  private def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().asScala.map(st => statement2triple(st)).toSet
  }

  private def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      JenaMapper.jenaNode2RDFNode(st.getSubject),
      property2iri(st.getPredicate),
      JenaMapper.jenaNode2RDFNode(st.getObject))
  }

  private def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  override def getPrefixMap: PrefixMap = {
    PrefixMap(
      model.getNsPrefixMap.asScala.toMap.map {
        case (alias, iri) => (Prefix(alias), IRI(iri))
      })
  }

  override def addPrefixMap(pm: PrefixMap): Rdf = {
    val map: Map[String, String] = pm.pm.map {
      case (Prefix(str), iri) => (str, iri.str)
    }
    model.setNsPrefixes(map.asJava)
    this
  }

  // TODO: Check that the last character is indeed :
  // private def removeLastColon(str: String): String = str.init

  override def addTriples(triples: Set[RDFTriple]): Either[String,Rdf] = {
    val newModel = JenaMapper.RDFTriples2Model(triples, model)
    model.add(newModel)
    Right(this)
  }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): Either[String,Rdf] = {
    val empty = ModelFactory.createDefaultModel
    val model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty)
    model.difference(model2delete)
    Right(this)
  }

  override def createBNode: (RDFNode, RDFAsJenaModel) = {
    val resource = model.createResource
    (BNode(resource.getId.getLabelString), this)
  }

  override def addPrefix(alias: String, iri: String): Rdf = {
    model.setNsPrefix(alias, iri)
    this
  }

/*  private def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  } */

  override def empty: Rdf = {
    RDFAsJenaModel.empty
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String, Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  /*private def resolveString(str: String): Either[String,IRI] = {
    Try(IRIResolver.resolveString(str)).fold(
      e => Left(e.getMessage),
      iri => Right(IRI(iri))
    )
  }*/
  private val NONE = "NONE"
  private val RDFS = "RDFS"
  private val OWL = "OWL"

  override def applyInference(inference: String): Either[String, Rdf] = {
    inference.toUpperCase match {
      case `NONE` => Right(this)
      case `RDFS` => JenaUtils.inference(model, RDFS).map(RDFAsJenaModel(_))
      case `OWL` => JenaUtils.inference(model, OWL).map(RDFAsJenaModel(_))
      case other => Left(s"Unsupported inference $other")
    }
  }

  def availableInferenceEngines: List[String] = List(NONE, RDFS, OWL)

  override def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]] = {
    val tryQuery: Try[List[Map[String,RDFNode]]] = Try {
    val qExec = QueryExecutionFactory.create(queryStr, model)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()
        // val varNames = result.getResultVars
        val ls: List[Map[String, RDFNode]] = result.asScala.toList.map(qs => {
          val qsm = new QuerySolutionMap()
          qsm.addAll(qs)
          qsm.asMap.asScala.toMap.mapValues(node => jenaNode2RDFNode(node))
        })
        ls
      }
      case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
    }
   }
   tryQuery.toEither.leftMap(_.getMessage)
  }

  override def queryAsJson(queryStr: String): Either[String, Json] = Try {
    val qExec = QueryExecutionFactory.create(queryStr, model)
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

  override def getNumberOfStatements(): Either[String,Int] =
    Right(model.size.toInt)

  override def isIsomorphicWith(other: RDFReader): Either[String,Boolean] = other match {
    case o: RDFAsJenaModel => Right(model.isIsomorphicWith(o.model))
    case _ => Left(s"Cannot compare RDFAsJenaModel with reader of different type: ${other.getClass.toString}")
  }

}


object RDFAsJenaModel {

  def apply(): RDFAsJenaModel = {
    RDFAsJenaModel.empty
  }

  lazy val empty: RDFAsJenaModel = {
    RDFAsJenaModel(ModelFactory.createDefaultModel)
  }

  def fromURI(uri: String, format: String = "TURTLE", base: Option[String] = None): Either[String,RDFAsJenaModel] = {
    val baseURI = base.getOrElse(FileUtils.currentFolderURL)
    Try {
      val m = ModelFactory.createDefaultModel()
      RDFDataMgr.read(m, uri, baseURI, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception accessing uri $uri: ${e.getMessage}"),
      Right(_)
    )
  }

  def fromFile(file: File, format: String, base: Option[String] = None): Either[String, RDFAsJenaModel] = {
    val baseURI = base.getOrElse("")
    Try {
      val m = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      RDFDataMgr.read(m, is, baseURI, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception parsing RDF from file ${file.getName}: ${e.getMessage}"),
           Right(_))
  }

  def fromChars(cs: CharSequence, format: String, base: Option[String] = None): Either[String,RDFAsJenaModel] = {
    RDFAsJenaModel.empty.fromString(cs, format, base)
  }

  def extractModel(rdf: RDFAsJenaModel): Model = {
    rdf match {
      case rdfJena: RDFAsJenaModel => rdfJena.model
      case _ => throw new Exception("Cannot extract Model from rdf:" + rdf)
    }
  }

  def availableFormats: List[String] = {
    RDFLanguages.getRegisteredLanguages().asScala.map(_.getName).toList.distinct
  }


}
