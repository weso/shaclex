package es.weso.rdf.rdf4j

import java.io._
import es.weso.rdf._
import es.weso.rdf.path.SHACLPath
import es.weso.rdf.triples.RDFTriple
import io.circe.Json
import org.eclipse.rdf4j.model.{
  IRI => IRI_RDF4j,
  Literal => Literal_RDF4j,
  _
}
import es.weso.rdf.nodes._
import org.eclipse.rdf4j.model.util.ModelBuilder
import org.eclipse.rdf4j.rio.RDFFormat._
import org.eclipse.rdf4j.rio.{RDFFormat, Rio}
import org.apache.commons.io.input.CharSequenceInputStream
import scala.util._
import scala.collection.JavaConverters._
import RDF4jMapper._

case class RDFAsRDF4jModel(model: Model)
  extends RDFReader
    with RDFBuilder
    with RDFReasoner {

  type Rdf = RDFAsRDF4jModel

  override def fromString(cs: CharSequence,
                          format: String,
                          base: Option[String] = None): Either[String, Rdf] = {
      val builder = new ModelBuilder()
      val baseURI = base.getOrElse("")
      for {
        format <- getRDFFormat(format)
        model <- Try {
          val is : InputStream = new CharSequenceInputStream(cs,"UTF-8")
          Rio.parse(is, baseURI, format)
        }.fold(e => Left(s"Exception: ${e.getMessage}\nBase:$base, format: $format\n$cs"),
          Right(_)
        )
   } yield RDFAsRDF4jModel(model)
  }

  private def getRDFFormat(name: String): Either[String,RDFFormat] = {
    name.toUpperCase match {
      case "TURTLE" => Right(TURTLE)
      case "JSONLD" => Right(JSONLD)
      case "RDFXML" => Right(RDFXML)
      case x => Left(s"Unsupported syntax $x")
    }
  }

  override def serialize(formatName: String): Either[String, String] = for {
    format <- getRDFFormat(formatName)
    str <- Try {
      val out: StringWriter = new StringWriter()
      Rio.write(model,out,format)
      out.toString
    }.fold(e => Left(s"Error serializing RDF to format $formatName: $e"),
      Right(_)
    )
  } yield str

  private def extend_rdfs: Rdf = {
    this
    // TODO: Check how to add inference in RDF4j
    /* val infModel = ModelFactory.createRDFSModel(model)
    RDFAsJenaModel(infModel) */
  }

  // TODO: this implementation only returns subjects
  override def iris(): Set[IRI] = {
    val resources: Set[Resource] = model.subjects().asScala.toSet
    resources.filter(_.isInstanceOf[IRI_RDF4j]).map(_.asInstanceOf[IRI_RDF4j].toString).map(IRI(_))
  }

  override def subjects(): Set[RDFNode] = {
    val resources: Set[Resource] = model.subjects().asScala.toSet
    resources.map(r => resource2RDFNode(r))
  }

  override def rdfTriples(): Set[RDFTriple] = {
    model.asScala.toSet.map(statement2RDFTriple(_))
  }

  override def triplesWithSubject(node: RDFNode): Set[RDFTriple] = {
    val maybeResource = rdfNode2Resource(node).toOption
    val empty: Set[RDFTriple] = Set()
    maybeResource.fold(empty) {
      case resource =>
        val statements: Set[Statement] = triplesSubject(resource, model)
        statements2RDFTriples(statements)
    }
  }

  /**
    * return the SHACL instances of a node `cls`
    * A node `node` is a shacl instance of `cls` if `node rdf:type/rdfs:subClassOf* cls`
    */
  override def getSHACLInstances(c: RDFNode): Seq[RDFNode] = {
    ???
    /*
    val cJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.getSHACLInstances(cJena, model).map(n => JenaMapper.jenaNode2RDFNode(n))
    */
  }

  override def hasSHACLClass(n: RDFNode, c: RDFNode): Boolean = {
    /*
    val nJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(n, model)
    val cJena: JenaRDFNode = JenaMapper.rdfNode2JenaNode(c, model)
    JenaUtils.hasClass(nJena, cJena, model)
    */
    ???
  }

  override def nodesWithPath(path: SHACLPath): Set[(RDFNode, RDFNode)] = {
    /*
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val pairs = JenaUtils.getNodesFromPath(jenaPath, model).
      map(p => (JenaMapper.jenaNode2RDFNode(p._1), JenaMapper.jenaNode2RDFNode(p._2)))
    pairs.toSet */
    ???
  }

  override def objectsWithPath(subj: RDFNode, path: SHACLPath): Set[RDFNode] = {
    ???
    /*
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(subj, model)
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val nodes = JenaUtils.objectsFromPath(jenaNode, jenaPath, model).map(n => JenaMapper.jenaNode2RDFNode(n))
    nodes.toSet
    */
  }

  override def subjectsWithPath(path: SHACLPath, obj: RDFNode): Set[RDFNode] = {
    ???
    /*
    val jenaNode: JenaRDFNode = JenaMapper.rdfNode2JenaNode(obj, model)
    val jenaPath: Path = JenaMapper.path2JenaPath(path, model)
    val nodes = JenaUtils.subjectsFromPath(jenaNode, jenaPath, model).map(n => JenaMapper.jenaNode2RDFNode(n))
    nodes.toSet
    */
  }

  /*def toRDFTriples(ls: Set[Statement]): Set[RDFTriple] = {
    ls.map(st => statement2triple(st))
  } */

  override def triplesWithPredicate(iri: IRI): Set[RDFTriple] = {
    val pred = iri2Property(iri)
    val statements: Set[Statement] = triplesPredicate(pred, model)
    statements2RDFTriples(statements)
  }

  override def triplesWithObject(node: RDFNode): Set[RDFTriple] = {
    val obj = rdfNode2Resource(node).toOption
    val empty: Set[RDFTriple] = Set()
    obj.fold(empty) { o => {
      val statements: Set[Statement] = triplesObject(o, model)
      statements2RDFTriples(statements)
    }
   }
  }

  override def triplesWithPredicateObject(p: IRI, o: RDFNode): Set[RDFTriple] = {
    ???
/*
    val pred = rdfNode2Property(p, model)
    val maybeObj = rdfNode2Resource(o, model)
    val empty: Set[RDFTriple] = Set()
    maybeObj.fold(empty) { obj =>
      val statements: Set[Statement] = triplesPredicateObject(pred, obj, model)
      toRDFTriples(statements)
    }
*/
  }

  override def getPrefixMap: PrefixMap = {
    PrefixMap {
      val nsSet: Set[Namespace] = model.getNamespaces.asScala.toSet
      nsSet.map(ns => (Prefix(ns.getPrefix), IRI(ns.getName))).toMap
    }
  }

  override def addPrefixMap(pm: PrefixMap): Rdf = {
    pm.pm.foreach {
      case (Prefix(prefix),value) => model.setNamespace(prefix,value.str)
    }
    this
  }

  override def addTriples(triples: Set[RDFTriple]): Rdf  = {
    ???
/*
    val newModel = JenaMapper.RDFTriples2Model(triples, model)
    model.add(newModel)
    this
*/
  }

  // TODO: This is not efficient
  override def rmTriple(triple: RDFTriple): Rdf = {
    ???
/*
    val empty = ModelFactory.createDefaultModel
    val model2delete = JenaMapper.RDFTriples2Model(Set(triple), empty)
    model.difference(model2delete)
    this
*/
  }

  override def createBNode: (RDFNode, Rdf) = {
    ???
/*
    val resource = model.createResource
    (BNodeId(resource.getId.getLabelString), this)
*/
  }

  override def addPrefix(alias: String, iri: String): Rdf = {
/*
    model.setNsPrefix(alias, iri)
    this
*/
    ???
  }

  /*def qName(str: String): IRI = {
    IRI(model.expandPrefix(str))
  }
*/
  override def empty: Rdf = {
    RDFAsRDF4jModel.empty
  }

  override def checkDatatype(node: RDFNode, datatype: IRI): Either[String, Boolean] =
    ???
/*
    JenaMapper.wellTypedDatatype(node, datatype)
*/

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
    Right(this)  // TODO (as it is doesn't apply inference)
/*
    inference.toUpperCase match {
      case `NONE` => Right(this)
      case `RDFS` => JenaUtils.inference(model, RDFS).map(RDFAsJenaModel(_))
      case `OWL` => JenaUtils.inference(model, OWL).map(RDFAsJenaModel(_))
      case other => Left(s"Unsupported inference $other")
    }
*/
  }

  def availableInferenceEngines: List[String] = List(NONE, RDFS, OWL)

  override def querySelect(queryStr: String): Either[String, List[Map[String,RDFNode]]] = {
    ???
/*
    val qExec = QueryExecutionFactory.create(queryStr, model)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()
        val varNames = result.getResultVars
        val ls: List[Map[String,RDFNode]] = result.asScala.toList.map(qs => {
          val qsm = new QuerySolutionMap()
          qsm.addAll(qs)
          qsm.asMap.asScala.toMap.mapValues(node => jenaNode2RDFNode(node))
        })
        Right(ls)
      }
      case qtype => Left(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
    }
*/
  }

  override def queryAsJson(queryStr: String): Either[String, Json] = ??? /*Try {
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
        val result = qExec.execConstruct()
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
  }.toEither.fold(f => Left(f.getMessage), es => es)*/


  override def getNumberOfStatements(): Either[String,Int] =
    Right(model.size.toInt)

}


object RDFAsRDF4jModel {

    def apply(): RDFAsRDF4jModel = {
    RDFAsRDF4jModel.empty
  }

  lazy val empty: RDFAsRDF4jModel = {
    val builder = new ModelBuilder()
    RDFAsRDF4jModel(builder.build)
  }

  def fromURI(uri: String, format: String = "TURTLE", base: Option[String] = None): Either[String,RDFAsRDF4jModel] = {
    ???
/*
    val baseURI = base.getOrElse(FileUtils.currentFolderURL)
    Try {
      val m = ModelFactory.createDefaultModel()
      RDFDataMgr.read(m, uri, baseURI, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception accessing uri $uri: ${e.getMessage}"),
      Right(_)
    )
*/
  }

  def fromFile(file: File, format: String, base: Option[String] = None): Either[String, RDFAsRDF4jModel] = {
    ???
/*
    val baseURI = base.getOrElse("")
    Try {
      val m = ModelFactory.createDefaultModel()
      val is: InputStream = new FileInputStream(file)
      RDFDataMgr.read(m, is, baseURI, shortnameToLang(format))
      RDFAsJenaModel(JenaUtils.relativizeModel(m))
    }.fold(e => Left(s"Exception parsing RDF from file ${file.getName}: ${e.getMessage}"),
      Right(_))
*/
  }

  def fromChars(cs: CharSequence, format: String, base: Option[String] = None): Either[String,RDFAsRDF4jModel] = {
    RDFAsRDF4jModel.empty.fromString(cs, format, base)
  }

  def availableFormats: List[String] = {
    val formats = List(TURTLE,JSONLD,RDFXML)
    formats.map(_.getName)
  }


}
