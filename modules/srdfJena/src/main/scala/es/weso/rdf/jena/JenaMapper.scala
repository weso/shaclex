package es.weso.rdf.jena

// TODO: Refactor this code
import org.apache.jena.rdf.model.{AnonId, ModelFactory, Property, Resource, Statement,
  Model => JenaModel,
  RDFNode => JenaRDFNode
}
import es.weso.rdf.nodes._
import org.apache.jena.datatypes.BaseDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import es.weso.rdf.triples.RDFTriple

import scala.collection.JavaConverters._
import com.typesafe.scalalogging._
import es.weso.rdf.path._
import org.apache.jena.sparql.path._

import util._

object JenaMapper {

  val logger = Logger("JenaMapper")

  lazy val emptyModel = ModelFactory.createDefaultModel()

  def RDFTriples2Model(triples: Set[RDFTriple], m: JenaModel): JenaModel = {
    for (t <- triples) {
      val subj = createResource(m, t.subj)
      val pred = createProperty(m, t.pred)
      val obj = createRDFNode(m, t.obj)
      val stmt = m.createStatement(subj, pred, obj)
      m.add(stmt)
    }
    m
  }

  def RDFTriple2Statement(triple: RDFTriple): Statement = {
    // TODO: implement
    throw new Exception("RDFTriple2Statement: unimplemented conversion from " + triple)
  }

  def statement2RDFTriple(s: Statement): RDFTriple = {
    val subj: RDFNode = jenaNode2RDFNode(s.getSubject)
    val pred: IRI = property2IRI(s.getPredicate)
    val obj: RDFNode = jenaNode2RDFNode(s.getObject)
    RDFTriple(subj, pred, obj)
  }

  private def resolve(iri: IRI): String =
    iri.str
    // IRIResolver.resolveString(iri.str)

  def rdfNode2Property(n: RDFNode, m: JenaModel): Property = {
    n match {
      case i: IRI => m.getProperty(resolve(i))
      case _ => throw new Exception("rdfNode2Property: unexpected node " + n)
    }
  }

  def rdfNode2Resource(n: RDFNode, m: JenaModel): Option[Resource] = {
    n match {
      case i: IRI => Some(m.getResource(resolve(i)))
      case BNode(id) => {
        // Creates the BNode if it doesn't exist
        Some(m.createResource(new AnonId(id)))
      }
      case _ => None
    }
  }

  def rdfNode2JenaNode(n: RDFNode, m: JenaModel): JenaRDFNode =
    createRDFNode(m, n)

  // TODO: Change this code to return an Either[String,RDFNode]
  def jenaNode2RDFNode(r: JenaRDFNode): RDFNode = {
    if (r.isURIResource()) {
      IRI(r.asResource().getURI)
    } else if (r.isAnon) {
      val b = BNode(r.asResource().getId.getLabelString)
      b
    } else if (r.isLiteral) {
      val lit = r.asLiteral()
      if (lit.getLanguage() != "") {
        LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
      } else {
        val maybeDatatype = lit.getDatatypeURI
        if (maybeDatatype == null) {
          StringLiteral(lit.getString())
        } else {
          val datatype = IRI(maybeDatatype)
          datatype match {
            case RDFNode.StringDatatypeIRI => StringLiteral(lit.getLexicalForm)
            case RDFNode.IntegerDatatypeIRI =>
              Try(IntegerLiteral(lit.getLexicalForm.toInt)).getOrElse {
                logger.error(s"LexicalForm ${lit.getLexicalForm()} can't be parsed as an integer to create literal")
                DatatypeLiteral(lit.getLexicalForm, datatype)
              }
            case RDFNode.DecimalDatatypeIRI =>
              Try(DecimalLiteral(lit.getLexicalForm.toDouble)).getOrElse {
                logger.error(s"LexicalForm ${lit.getLexicalForm()} can't be parsed as a decimal to create literal")
                DatatypeLiteral(lit.getLexicalForm, datatype)
              }
            case RDFNode.BooleanDatatypeIRI =>
              Try(BooleanLiteral(lit.getLexicalForm.toBoolean)).getOrElse {
                logger.error(s"LexicalForm ${lit.getLexicalForm()} can't be parsed as boolean to create literal")
                DatatypeLiteral(lit.getLexicalForm, datatype)
              }
            case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
            case _ => DatatypeLiteral(lit.getLexicalForm, datatype)
          }
        }
      }
    } else throw new Exception(s"resource2RDFNode: unexpected type of resource: $r")
  }

  def property2IRI(p: Property): IRI = IRI(p.getURI)

  def createResource(m: JenaModel, node: RDFNode): Resource = {
    node match {
      case BNode(id) => m.createResource(new AnonId(id.toString))
      case i: IRI => m.createResource(resolve(i))
      case _ => throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createRDFNode(m: JenaModel, node: RDFNode): JenaRDFNode = {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val xsdinteger = xsd + "integer"
    val xsddouble = xsd + "double"
    val xsddecimal = xsd + "decimal"
    val xsdboolean = xsd + "boolean"

    node match {
      case BNode(id) =>
        m.createResource(new AnonId(id.toString))
      case i: IRI =>
        m.createResource(resolve(i))
      case StringLiteral(str) =>
        m.createLiteral(str, false)
      case DatatypeLiteral(str, i: IRI) =>
        i.str match {
          case `xsdinteger` => m.createTypedLiteral(str, XSDDatatype.XSDinteger)
          case `xsddouble` => m.createTypedLiteral(str, XSDDatatype.XSDdouble)
          case `xsddecimal` => m.createTypedLiteral(str, XSDDatatype.XSDdecimal)
          case `xsdboolean` => m.createTypedLiteral(str, XSDDatatype.XSDboolean)
          case _ => m.createTypedLiteral(str, new BaseDatatype(i.str))
        }
      case DecimalLiteral(d) =>
        m.createTypedLiteral(d.toString(), XSDDatatype.XSDdecimal)
      case IntegerLiteral(i) =>
        m.createTypedLiteral(i.toString, XSDDatatype.XSDinteger)
      case LangLiteral(l, Lang(lang)) => m.createLiteral(l, lang)
      case BooleanLiteral(b) =>
        m.createTypedLiteral(b.toString, XSDDatatype.XSDboolean)
      case DoubleLiteral(d: Double) =>
        m.createTypedLiteral(d.toString, XSDDatatype.XSDdouble)
      case _ =>
        throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createProperty(m: JenaModel, pred: IRI): Property = {
    m.createProperty(resolve(pred))
  }

  def triplesSubject(resource: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(resource, null, null).toSet.asScala.toSet
  }

  def triplesSubjectPredicate(resource: Resource, pred: IRI, model: JenaModel): Set[Statement] = {
    model.listStatements(resource, createProperty(model,pred), null).toSet.asScala.toSet
  }

  def triplesPredicateObject(pred: IRI, resource: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, createProperty(model,pred), resource).toSet.asScala.toSet
  }

  def triplesPredicate(pred: Property, model: JenaModel): Set[Statement] = {
    model.listStatements(null, pred, null).toSet.asScala.toSet
  }

  def triplesObject(obj: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, null, obj).toSet.asScala.toSet
  }

  def triplesPredicateObject(property: Property, obj: Resource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, property, obj).toSet.asScala.toSet
  }

  // TODO: Return Either[String,Path]
  def path2JenaPath(path: SHACLPath, model: JenaModel): Path = {
    path match {
      case PredicatePath(iri) => {
        val prop = rdfNode2Property(iri, model)
        new P_Link(prop.asNode)
      }
      case InversePath(path) => {
        val jenaPath = path2JenaPath(path, model)
        new P_Inverse(jenaPath)
      }
      case SequencePath(paths) => {
        val jenaPaths = paths.map(path => path2JenaPath(path, model))
        def seq(p1: Path, p2: Path): Path = new P_Seq(p1, p2)
        jenaPaths.reduce(seq)
      }
      case AlternativePath(paths) => {
        val jenaPaths = paths.map(path => path2JenaPath(path, model))
        def alt(p1: Path, p2: Path): Path = new P_Alt(p1, p2)
        jenaPaths.reduce(alt)
      }
      case ZeroOrMorePath(path) => {
        val jenaPath = path2JenaPath(path, model)
        new P_ZeroOrMoreN(jenaPath)
      }
      case ZeroOrOnePath(path) => {
        val jenaPath = path2JenaPath(path, model)
        new P_ZeroOrOne(jenaPath)
      }
      case OneOrMorePath(path) => {
        val jenaPath = path2JenaPath(path, model)
        new P_OneOrMoreN(jenaPath)
      }
    }
  }

  def wellTypedDatatype(node: RDFNode, expectedDatatype: IRI): Either[String, Boolean] = {
    node match {
      case l: es.weso.rdf.nodes.Literal => {
        Try {
          val jenaLiteral = emptyModel.createTypedLiteral(l.getLexicalForm, l.dataType.str)
          jenaLiteral.getValue // if it is ill-typed it raises an exception
          jenaLiteral.getDatatypeURI
        } match {
          case Success(iri) => {
            // println(s"JenaMapper.welltypedDatatype, $node. Comparing $expectedDatatype with $iri")
            Right(iri == expectedDatatype.str)
          }
          case Failure(e) => Left(e.getMessage)
        }
      }
      case _ => Right(false)
    }
  }

}
