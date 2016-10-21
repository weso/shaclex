package es.weso.rdf.jena

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.triples.RDFTriple
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdf.triples._
import es.weso.rdf._
import org.apache.jena.rdf.model.{
  Model,
  Resource,
  Property,
  Statement,
  RDFNode => JenaRDFNode,
  RDFReader => JenaRDFReader,
  StmtIterator,
  ModelFactory
}
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
import es.weso.utils.JenaUtils

case class RDFAsJenaModel(model: Model)
    extends RDFReader
    with RDFBuilder {

  type Rdf = RDFAsJenaModel

  val log = LoggerFactory.getLogger("RDFAsJenaModel")

  override def parse(cs: CharSequence, format: String = "TURTLE", base: Option[String] = None): Try[RDFAsJenaModel] = {
    try {
      val m = ModelFactory.createDefaultModel
      val str_reader = new StringReader(cs.toString)
      val baseURI = base.getOrElse("")
      RDFDataMgr.read(m, str_reader, baseURI, shortnameToLang(format))
      Success(RDFAsJenaModel(m))
    } catch {
      case e: Exception => Failure(new Exception(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs" ))
    }
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
    val resources: Set[Resource] = model.listSubjects().toSet().toSet
    resources.filter(s => s.isURIResource).map(r => IRI(r.getURI))
  }

  override def subjects(): Set[RDFNode] = {
    val resources: Set[Resource] = model.listSubjects().toSet().toSet
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
    val cJena : JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.getSHACLInstances(cJena,model).map(n => JenaMapper.jenaNode2RDFNode(n))
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Boolean = {
    val nJena : JenaRDFNode = JenaMapper.rdfNode2JenaNode(n, model)
    val cJena : JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.hasClass(nJena,cJena,model)
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

  // TODO: Check if it can be optimized in Jena 
  /*  override def triplesWithType(expectedType:IRI): Set[RDFTriple] = {
    triplesWithPredicateObject(rdf_type,expectedType)
  } */

  def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().map(st => statement2triple(st)).toSet
  }

  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
      JenaMapper.jenaNode2RDFNode(st.getSubject),
      property2iri(st.getPredicate),
      JenaMapper.jenaNode2RDFNode(st.getObject)
    )
  }

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }

  override def getPrefixMap: PrefixMap = {
    PrefixMap(
      model.getNsPrefixMap.toMap.map {
        case (alias, iri) => (Prefix(alias + ":"), IRI(iri))
      }
    )
  }

  override def addPrefixMap(pm: PrefixMap): RDFAsJenaModel = {
    val map: Map[String, String] = pm.pm.map {
      case (Prefix(str), iri) => (str, iri.str) }
    model.setNsPrefixes(map)
    this
  }

  // TODO: Check that the last character is indeed :
  def removeLastColon(str: String): String = str.init

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

}

object RDFAsJenaModel {

  def apply(): RDFAsJenaModel = {
    RDFAsJenaModel.empty
  }

  def empty: RDFAsJenaModel = {
    RDFAsJenaModel(ModelFactory.createDefaultModel)
  }

  def fromURI(uri: String, format: String = "TURTLE", base: Option[String] = None): Try[RDFAsJenaModel] = {
    val baseURI = base.getOrElse("")
    try {
      val m = ModelFactory.createDefaultModel()
      RDFDataMgr.read(m, uri, baseURI, shortnameToLang(format))
      Success(RDFAsJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + uri + ": " + e.getMessage))
    }
  }

  def fromFile(file: File, format: String, base: Option[String] = None): Try[RDFAsJenaModel] = {
    val baseURI = base.getOrElse("")
    try {
      val m = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      RDFDataMgr.read(m, is, baseURI, shortnameToLang(format))
      Success(RDFAsJenaModel(m))
    } catch {
      case e: Exception => Failure(throw new Exception("Exception accessing  " + file.getName + ": " + e.getMessage))
    }
  }

  def fromChars(cs: CharSequence, format: String, base: Option[String] = None): Try[RDFAsJenaModel] = {
    try {
      RDFAsJenaModel.empty.parse(cs, format, base)
    } catch {
      case e: Exception => Failure(throw new Exception("Exception reading  " + formatLines(cs.toString) + "\n " + e.getMessage))
    }
  }

  def formatLines(cs: CharSequence): String = {
    cs.toString.lines.zipWithIndex.map(p => (p._2 + 1).toString + " " + p._1).mkString("\n")
  }

  def extractModel(rdf: RDFAsJenaModel): Model = {
    rdf match {
      case rdfJena: RDFAsJenaModel => rdfJena.model
      case _ => throw new Exception("Cannot extract Model from rdf:" + rdf)
    }
  }

  def availableFormats: List[String] = {
    RDFLanguages.getRegisteredLanguages().map(_.getName).toList.distinct
  }


}
