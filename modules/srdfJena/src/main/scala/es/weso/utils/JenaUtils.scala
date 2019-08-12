package es.weso.utils

import org.apache.jena.rdf.model.{Literal, Model, ModelFactory, Property, RDFNode, Resource, SimpleSelector}
import org.apache.jena.sparql.syntax.ElementPathBlock
import org.apache.jena.riot.system.IRIResolver
import java.io.ByteArrayInputStream

import org.apache.jena.query.{Query, QueryExecutionFactory, QueryFactory, QuerySolution, ResultSet}
import java.io.StringWriter
import java.net.URI
import java.net.URL
import java.io.InputStream
import java.io.FileOutputStream

import org.apache.jena.atlas.AtlasException
import org.apache.jena.riot.RiotException

import scala.collection.JavaConverters._
import org.apache.jena.reasoner.ReasonerRegistry
import org.apache.jena.sparql.core.{TriplePath, Var}
import org.apache.jena.sparql.path.Path
import org.apache.jena.util.{FileUtils => FileJenaUtils}

import scala.annotation.tailrec
import scala.util.Try
import cats.syntax.either._
import org.apache.jena.shared.PrefixMapping

object JenaUtils {

  lazy val RdfXML = FileJenaUtils.langXML
  lazy val RdfXMLAbbr = FileJenaUtils.langXMLAbbrev
  lazy val NTriple = FileJenaUtils.langNTriple
  lazy val Turtle = FileJenaUtils.langTurtle
  lazy val TTL = "TTL"
  lazy val N3 = "N3"

  // In Jena selectors, null represents any node
  lazy val any: Resource = null

  def extractModel(resource: Resource, model: Model): Model = {
    val nModel = ModelFactory.createDefaultModel()

    def inner(resource: Resource): Model = {
      val iterator2 = model.listStatements(resource, null, null)

      while (iterator2.hasNext()) {
        val stmt = iterator2.nextStatement();
        val subject = stmt.getSubject();
        val predicate = stmt.getPredicate();
        val objec = stmt.getObject();
        nModel.add(subject, predicate, objec)
        if (objec.isAnon) {
          inner(objec.asResource())
        }
      }
      nModel
    }
    inner(resource)
  }

  def dereferenceURI(uri: String): InputStream = {
    val url = new URL(uri)
    val urlCon = url.openConnection()
    urlCon.setConnectTimeout(4000)
    urlCon.setReadTimeout(2000)
    urlCon.getInputStream()
  }

  def parseFromURI(
    uri: String,
    base: String = "",
    syntax: String = Turtle): Either[String,Model] = {
    uri2Model(uri, base, syntax) match {
      case Parsed(model) => Right(model)
      case NotParsed(err) => Left(err)
    }
  }

  def parseFromString(
    content: String,
    base: String = "",
    syntax: String = Turtle): Either[String,Model] = {
    str2Model(content, base, syntax) match {
      case Parsed(model) => Right(model)
      case NotParsed(err) =>Left("Cannot parse from string: " + content + "\nError: " + err + "\nSyntax: " + syntax)
    }
  }

  def uri2Model(
    uriName: String,
    base: String = "",
    syntax: String = Turtle): ParserReport[Model, String] = {
    try {
      val model = ModelFactory.createDefaultModel()
      Parsed(model.read(dereferenceURI(uriName), base, syntax))
    } catch {
      case e: AtlasException =>
        NotParsed("Error parsing URI " + uriName + " with syntax " + syntax + ".\n AtlasException: " + e.toString())
      case e: RiotException =>
        NotParsed("Exception parsing URI " + uriName + " with syntax " + syntax + ".\n RIOT Exception: " + e.toString())
      case e: Exception =>
        NotParsed("Exception parsing URI " + uriName + " with syntax " + syntax + ".\n Exception: " + e.toString())
    }
  }

  /**
   * Returns a RDF model after parsing a String
   */
  def str2Model(
    str: String,
    base: String = "",
    syntax: String = Turtle): ParserReport[Model, String] = {
    try {
      val model = ModelFactory.createDefaultModel()
      val stream = new ByteArrayInputStream(str.getBytes("UTF-8"))
      Parsed(model.read(stream, base, syntax))
    } catch {
      case e @ (_: AtlasException | _: RiotException) =>
        NotParsed("Bad formed with syntax " + syntax + ". " + e.getLocalizedMessage())
      case e: Exception =>
        NotParsed("Exception parsing from String " + str +
          " with syntax " + syntax + ". " + e.getLocalizedMessage())
    }
  }

  /**
   * Returns a RDF model after parsing an InputStream
   */
  def parseInputStream(
    stream: InputStream,
    base: String = "",
    syntax: String = Turtle): ParserReport[Model, String] = {
    try {
      val model = ModelFactory.createDefaultModel()
      Parsed(model.read(stream, base, syntax))
    } catch {
      case e @ (_: AtlasException | _: RiotException) =>
        NotParsed("Bad formed with syntax " + syntax + ". " + e.getLocalizedMessage())
      case e: Exception =>
        NotParsed("Exception parsing " +
          " with syntax " + syntax + ". " + e.getLocalizedMessage())
    }
  }

  def getLiteral(
    r: RDFNode,
    property: Property): String = {
    if (r.isResource()) {
      val res = r.asResource
      val stmt = res.getRequiredProperty(property)
      stmt match {
        case null =>
          throw new Exception("getName: " + res + " doesn't have value for property " + property + ".\n" +
            showResource(r.asResource))
        case _ =>
          if (stmt.getObject.isLiteral) stmt.getObject.asLiteral.getString
          else
            throw new Exception("getName: " + stmt.getObject + " is not a literal")
      }
    } else
      throw new Exception("getName: " + r + "is not a resource")
  }

  /*
   *
   */
  def getURI(r: RDFNode): URI = {
    if (r.isResource) {
      new URI(r.asResource.getURI)
    } else
      throw new Exception("getURI: Node " + r + " is not a resource")
  }

  /*
   * If there is a triple <r,p,u> and u is a URI, returns u
   * @param r RDFNode
   * @param property
   */
  def getObjectURI(
    r: RDFNode,
    p: Property): URI = {
    if (r.isResource()) {
      val resUri = r.asResource().getPropertyResourceValue(p)
      resUri match {
        case null =>
          throw new Exception("getURI: " + resUri + " doesn't have value for property " + p + ".\n" + showResource(r.asResource))
        case _ =>
          getURI(resUri)
      }
    } else
      throw new Exception("getURI: Node " + r + " is not a resource")
  }

  def getResource(node: RDFNode): Option[Resource] = {
    if (node.isResource()) Some(node.asResource())
    else None
  }

  /**
   * Given a class `cls`, obtains all nodes such as `node rdf:type/rdfs:subClassOf* cls`
   */
  def getSHACLInstances(cls: RDFNode, model: Model): Either[String, Seq[RDFNode]] = {
    for {
      /*
    val pss: ParameterizedSparqlString = new ParameterizedSparqlString()
    pss.setNsPrefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    pss.setNsPrefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
    pss.setCommandText("SELECT ?n { ?n rdf:type/rdfs:subClassOf* ?c . }")
    pss.setParam("c", cls)
    val result = QueryExecutionFactory.create(pss.asQuery, model).execSelect
    result.asScala.toSeq.map(qs => qs.get("n")) */
      is <- getDirectInstances(cls, model)
      subClss <- EitherUtils
        .sequence(getAllSubClasses(cls, model).toList.map(c => getDirectInstances(c, model)))
        .map(_.flatten.toSeq)
    } yield is.toSeq ++ subClss
  }

  /**
   * Checks is a `node rdf:type/rdfs:subClassOf* cls`
   */
  def hasClass(n: RDFNode, c: RDFNode, model: Model): Boolean = {
    getSHACLTypes(n, model).exists(sameNodeAs(_,c))
  }

  lazy val rdfTypeUrl = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  lazy val subClassOfUrl = "http://www.w3.org/2000/01/rdf-schema#subClassOf"

  def getSHACLTypes(n: RDFNode, model: Model): Set[RDFNode] = {
    extendWithSuperClasses(getDirectTypes(n,model).toList,model)
  }

  def getDirectTypes(n: RDFNode, model: Model): Set[RDFNode] = {
    if (n.isResource) {
      val rdfType = model.getProperty(rdfTypeUrl)
      val selector = new SimpleSelector(n.asResource(), rdfType, any)
      model.listStatements(selector).asScala.map(_.getObject).toSet
    } else Set()
  }

  def getDirectInstances(n: RDFNode, model: Model): Either[String, Set[Resource]] = Try {
      val rdfType = model.getProperty(rdfTypeUrl)
      val selector = new SimpleSelector(any, rdfType, n)
      val s: Set[Resource] = model.listStatements(selector).asScala.map(_.getSubject).toSet
      s
  }.fold(
    e => Left(s"getDirectInstances: ${e.getMessage}"),
    (s: Set[Resource]) => s.asRight[String]
  )

  def getSuperClasses(c: RDFNode, model: Model): Set[RDFNode] = {
    if (c.isResource) {
      val subClassOf = model.getProperty(subClassOfUrl)
      val selector = new SimpleSelector(c.asResource(), subClassOf, any)
      model.listStatements(selector).asScala.map(_.getObject).toSet
    } else Set()
  }

  private def extendWithSuperClasses(types: List[RDFNode], model: Model): Set[RDFNode] = {
    extendWithSuperClassesAux(types, model, Set[RDFNode]())
  }


  private def getAllSubClasses(cls: RDFNode,model: Model): Set[RDFNode] = {
    val directSubclasses = getDirectSubclasses(cls,model)
    getAllSubClassesAux(directSubclasses.toList,model, Set())
  }

  private def getDirectSubclasses(cls: RDFNode, model: Model): Set[RDFNode] = {
    val subClassOf = model.getProperty(subClassOfUrl)
    val selector = new SimpleSelector(any, subClassOf, cls)
    model.listStatements(selector).asScala.map(_.getSubject).toSet
  }

  @tailrec
  private def getAllSubClassesAux(cls: List[RDFNode], model: Model, visited: Set[RDFNode]): Set[RDFNode] = {
    cls match {
      case Nil => visited
      case c :: cs => if (visited.contains(c))
        getAllSubClassesAux(cs,model,visited)
      else
        getAllSubClassesAux(getDirectSubclasses(c,model).toList,model,visited + c)
    }
  }

  @tailrec
  private def extendWithSuperClassesAux(types: List[RDFNode], model: Model, visited: Set[RDFNode]): Set[RDFNode] = {
    types match {
      case Nil => visited
      case t :: ts =>
          if (visited.contains(t))
            extendWithSuperClassesAux(ts, model, visited)
          else
            extendWithSuperClassesAux(getSuperClasses(t, model).toList ++ ts, model, visited + t)
    }
  }

  def sameNodeAs(v1: RDFNode, v2: RDFNode): Boolean = {
    (v1,v2) match {
      case (r1: Resource, r2: Resource) if r1.isURIResource && r2.isURIResource => r1.getURI == r2.getURI
      case (r1:Resource,r2: Resource) if r1.isAnon && r2.isAnon => r1.getId == r2.getId
      case (b1: Literal, b2: Literal) => b1.getLexicalForm == b2.getLexicalForm
      case (_,_) => false
    }
  }

  def getNodesFromPath(path: Path, model: Model): Seq[(RDFNode, RDFNode)] = {
    // Build the following query:
    // SELECT ?sub ?obj { ?sub ?path ?obj }
    val query: Query = QueryFactory.make()
    query.setQuerySelectType()
    val sub = Var.alloc("sub")
    val obj = Var.alloc("obj")
    val e = new ElementPathBlock()
    e.addTriple(new TriplePath(sub, path, obj))
    query.addResultVar(sub)
    query.addResultVar(obj)
    query.setQueryPattern(e)
    val result = QueryExecutionFactory.create(query, model).execSelect
    result.asScala.toSeq.map(qs => (qs.get("sub"), qs.get("obj")))
  }

  def objectsFromPath(subj: RDFNode, path: Path, model: Model): Seq[RDFNode] = {
    // Build the following query:
    // SELECT ?obj { ?n ?path ?obj }
    val query: Query = QueryFactory.make()
    query.setQuerySelectType()
    val obj = Var.alloc("obj")
    val e = new ElementPathBlock()
    e.addTriple(new TriplePath(subj.asNode, path, obj))
    query.addResultVar(obj)
    query.setQueryPattern(e)
    val result = QueryExecutionFactory.create(query, model).execSelect
    result.asScala.toSeq.map(qs => qs.get("obj"))
  }

  def subjectsFromPath(obj: RDFNode, path: Path, model: Model): Seq[RDFNode] = {
    // Build the following query:
    // SELECT ?sub { ?sub ?path ?obj }
    val query: Query = QueryFactory.make()
    query.setQuerySelectType()
    val subj = Var.alloc("subj")
    val e = new ElementPathBlock()
    e.addTriple(new TriplePath(subj, path, obj.asNode))
    query.addResultVar(subj)
    query.setQueryPattern(e)
    val result = QueryExecutionFactory.create(query, model).execSelect
    result.asScala.toSeq.map(qs => qs.get("subj"))
  }

  /**
   * Shows infomation about a resource (list all the statements)
   */
  def showResource(resource: Resource): String = {
    val sb = new StringBuilder
    val iter = resource.listProperties()
    sb ++= ("Infor about: " + resource + "\n")
    while (iter.hasNext) {
      val st = iter.next
      sb ++= (st.toString + "\n")
    }
    sb.toString
  }

  /*
   * Parse a string to obtain a query
   */
  /*def parseQuery(
    str: String): Option[Query] = {
    try {
      val query = QueryFactory.create(str)
      Some(query)
    } catch {
      case _: Exception => None
    }
  } */

  def querySelectModel(query: Query, model: Model): ResultSet = {
    val qexec = QueryExecutionFactory.create(query, model)
    qexec.execSelect()
  }

  def querySelectModel(queryStr: String, model: Model): ResultSet = {
    val query = QueryFactory.create(queryStr)
    querySelectModel(query, model)
  }

  def queryConstructModel(queryStr: String, model: Model): Model = {
    val query = QueryFactory.create(queryStr)
    queryConstructModel(query, model)
  }

  def queryConstructModel(query: Query, model: Model): Model = {
    // val resultModel = ModelFactory.createDefaultModel
    val qexec = QueryExecutionFactory.create(query, model)
    qexec.execConstruct
  }

  /*
   * Convert a model to a String
   */
  def model2Str(
    model: Model,
    syntax: String = Turtle): String = {
    val strWriter = new StringWriter
    model.write(strWriter, syntax)
    strWriter.toString
  }

  /*
   * Write a model to a file
   */
  def model2File(
    model: Model,
    fileName: String,
    syntax: String = Turtle): Unit = {
    model.write(new FileOutputStream(fileName), syntax)
    ()
  }

  /* def getValuesOfType(r: Resource, m: Model) : Set[Resource] = {
  m.listResourcesWithProperty(rdf_type,r).toSet.asScala.toSet
 } */

  def findProperty(m: Model, r: Resource, p: Property): RDFNode = {
    val iter = m.listStatements(r, p, any)
    if (iter.hasNext) {
      val node = iter.next.getObject
      if (!iter.hasNext) node
      else throw
        new Exception("findProperty: Resource " + r + " has more than one value for property " + p)
    } else
      throw new Exception("findPropery: Resource " + r + " does not have value for property " + p)
  }

  def findProperty_asResource(m: Model, r: Resource, p: Property): Resource = {
    val v = findProperty(m, r, p)
    if (v.isResource) v.asResource
    else {
      throw new Exception("findProperty_asResource: Resource " + r + " has value " + v + " for property " + p + " which is not a resource")
    }
  }

  def findProperty_asLiteral(m: Model, r: Resource, p: Property): Literal = {
    val v = findProperty(m, r, p)
    if (v.isLiteral) v.asLiteral
    else {
      throw new Exception("findProperty_asLiteral: Resource " + r + " has value " + v + " for property " + p + " which is not a literal")
    }
  }

  def inference(rdf: Model, inference: String): Either[String, Model] = {
    inference match {
      case "RDFS" => {
        val inf = ModelFactory.createRDFSModel(rdf)
        Right(inf)
      }
      case "OWL" => {
        val reasoner = ReasonerRegistry.getOWLReasoner();
        val inf = ModelFactory.createInfModel(reasoner,rdf)
        Right(inf)
      }
      case _ => Left(s"Unsupported inference $inference")
    }
  }

  def relativizeStr(str: String, base: Option[URI]): String = {
    val baseURI = base.getOrElse(IRIResolver.chooseBaseURI.toURI)
    baseURI.relativize(new URI(str)).toString
  }

  def relativizeNode(m: Model, n: RDFNode, base: Option[URI]): RDFNode = {
    n match {
      case _ if n.isResource => relativizeResource(m, n.asResource(),base)
      case _ => n
    }
  }

  def relativizeResource(m: Model, r: Resource, base: Option[URI]): Resource = {
    r match {
      case _ if r.isURIResource => m.createResource(relativizeStr(r.getURI, base))
      case _ => r
    }
  }

  def relativizeProperty(m: Model, r: Property, base: Option[URI]): Property = {
    m.createProperty(relativizeStr(r.getURI(),base))
  }

  def relativizeModel(m: Model, base: Option[URI]):Model = {
    val r: Model = ModelFactory.createDefaultModel()
    r.setNsPrefixes(m)
    for (s <- m.listStatements().asScala) {
      val subj = relativizeResource(m, s.getSubject, base)
      val prop = relativizeProperty(r, s.getPredicate,base)
      val obj = relativizeNode(m, s.getObject,base)
      r.add(subj,prop,obj)
    }
    r
  }

}
