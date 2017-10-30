package es.weso.rdf.jena

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple
import org.apache.jena.riot.system.IRIResolver

import scala.collection.JavaConverters._
import scala.util.Try
import es.weso.rdf.triples._
import es.weso.rdf._
import org.apache.jena.rdf.model.{ Model, ModelFactory, Property, Resource, Statement, StmtIterator, RDFNode => JenaRDFNode, RDFReader => JenaRDFReader }
import org.slf4j._
import org.apache.jena.riot.{ Lang => JenaLang }
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.rdf.model.ModelFactory
import java.io._

import scala.util._
import java.io._

import es.weso.rdf.jena.SPARQLQueries._
import org.apache.jena.riot.RDFLanguages._
import org.apache.jena.riot.RDFLanguages
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path.SHACLPath
import es.weso.utils._
import org.apache.jena.sparql.path.Path

case class RDFAsJenaModel(model: Model)
  extends RDFReader
  with RDFBuilder
  with RDFReasoner {

  type Rdf = RDFAsJenaModel

  val log = LoggerFactory.getLogger("RDFAsJenaModel")

  override def parse(cs: CharSequence,
                     format: String = "TURTLE",
                     base: Option[String] = None): Either[String, RDFAsJenaModel] = {
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

  override def serialize(format: String): String = {
    val out: StringWriter = new StringWriter()
    model.write(out, format)
    out.toString
  }

  def extend_rdfs: Rdf = {
    val infModel = ModelFactory.createRDFSModel(model)
    RDFAsJenaModel(infModel)
  }

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

  def toRDFTriples(ls: Set[Statement]): Set[RDFTriple] = {
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
    obj.fold(empty) { o =>
      {
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

  def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().asScala.map(st => statement2triple(st)).toSet
  }

  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      JenaMapper.jenaNode2RDFNode(st.getSubject),
      property2iri(st.getPredicate),
      JenaMapper.jenaNode2RDFNode(st.getObject))
  }

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  override def getPrefixMap: PrefixMap = {
    PrefixMap(
      model.getNsPrefixMap.asScala.toMap.map {
        case (alias, iri) => (Prefix(alias), IRI(iri))
      })
  }

  override def addPrefixMap(pm: PrefixMap): RDFAsJenaModel = {
    val map: Map[String, String] = pm.pm.map {
      case (Prefix(str), iri) => (str, iri.str)
    }
    model.setNsPrefixes(map.asJava)
    this
  }

  // TODO: Check that the last character is indeed :
  private def removeLastColon(str: String): String = str.init

  override def addTriples(triples: Set[RDFTriple]): RDFAsJenaModel = {
    val newModel = JenaMapper.RDFTriples2Model(triples, model)
    model.add(newModel)
    this
  }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): RDFAsJenaModel = {
    val empty = ModelFactory.createDefaultModel
    val model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty)
    model.difference(model2delete)
    this
  }

  override def createBNode: (RDFNode, RDFAsJenaModel) = {
    val resource = model.createResource
    (BNodeId(resource.getId.getLabelString), this)
  }

  override def addPrefix(alias: String, iri: String): RDFAsJenaModel = {
    model.setNsPrefix(alias, iri)
    this
  }

  def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  }

  def empty: Rdf = {
    RDFAsJenaModel.empty
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String,Boolean] =
    JenaMapper.wellTypedDatatype(node, datatype)

  private def resolveString(str: String): Either[String,IRI] = {
    Try(IRIResolver.resolveString(str)).fold(
      e => Left(e.getMessage),
      iri => Right(IRI(iri))
    )
  }

  def applyInference(inference: String): Either[String, Rdf] = {
    println(s"############## Inference $inference")
    inference.toUpperCase match {
      case "NONE" => Right(this)
      case "RDFS" => JenaUtils.inference(model, "RDFS").map(RDFAsJenaModel(_))
      case other => Left(s"Unsupported inference $other")
    }
  }

  def availableInferenceEngines: List[String] = List("None", "RDFS")

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
      RDFDataMgr.read(m, is, null, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception parsing RDF from file ${file.getName}: ${e.getMessage}"),
           Right(_))
  }

  def fromChars(cs: CharSequence, format: String, base: Option[String] = None): Either[String,RDFAsJenaModel] = {
    RDFAsJenaModel.empty.parse(cs, format, base)
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
