package es.weso.rdf.jena

// TODO: Refactor this code
import org.apache.jena.rdf.model.{
  AnonId,
  ModelFactory,
  Property,
  Resource => JenaResource,
  Statement,
  Model => JenaModel,
  Literal => JenaLiteral,
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

  def RDFTriples2Model(triples: Set[RDFTriple],
                       m: JenaModel,
                       base: Option[IRI]
                      ): JenaModel = {
    for (t <- triples) {
      val subj = createResource(m, t.subj,base)
      val pred = createProperty(m, t.pred,base)
      val obj = createRDFNode(m, t.obj,base)
      val stmt = m.createStatement(subj, pred, obj)
      m.add(stmt)
    }
    m
  }

  def RDFTriple2Statement(triple: RDFTriple): Statement = {
    // TODO: implement
    throw new Exception("RDFTriple2Statement: unimplemented conversion from " + triple)
  }

  def statement2RDFTriple(s: Statement): Either[String,RDFTriple] = for {
    subj <- jenaNode2RDFNode(s.getSubject)
    pred = property2IRI(s.getPredicate)
    obj <- jenaNode2RDFNode(s.getObject)
  } yield
    RDFTriple(subj, pred, obj)

  private def resolve(iri: IRI, base: Option[IRI]): String = base match {
    case None => iri.str
    case Some(baseIri) => baseIri.resolve(iri).str
  }

  def rdfNode2Property(n: RDFNode,
                       m: JenaModel,
                       base: Option[IRI]
                      ): Property = {
    n match {
      case i: IRI => m.getProperty(resolve(i,base))
      case _ => throw new Exception("rdfNode2Property: unexpected node " + n)
    }
  }

  def rdfNode2Resource(n: RDFNode,
                       m: JenaModel,
                       base: Option[IRI]): Option[JenaResource] = {
    n match {
      case i: IRI => Some(m.getResource(resolve(i,base)))
      case BNode(id) => {
        // Creates the BNode if it doesn't exist
        Some(m.createResource(new AnonId(id)))
      }
      case _ => None
    }
  }

  def rdfNode2JenaNode(n: RDFNode, m: JenaModel, base: Option[IRI]): JenaRDFNode =
    createRDFNode(m, n, base)

  def jenaNode2RDFNodeUnsafe(r: JenaRDFNode): RDFNode = {
    jenaNode2RDFNode(r).fold(e => StringLiteral(s"Error: $e"), identity)
  }

  /**
  * If str is "xsd:year" returns http://www.w3.org/2001/XMLSchema#
    * @param str
    * @return
    */
  private def extendNS(str: String): Option[IRI] = {
    val xsdIRI = IRI("http://www.w3.org/2001/XMLSchema#")
    val xsdRegex = raw"xsd\:([A-Za-z]+)".r
    str match {
      case xsdRegex(name) => Some(xsdIRI.add(name))
      case other => IRI.fromString(other,None).toOption
    }
  }

  // TODO: Change this code to return an Either[String,RDFNode]
  def jenaNode2RDFNode(r: JenaRDFNode): Either[String,RDFNode] = r match {
    case _ if r.isAnon =>  {
      val b = BNode(r.asResource().getId.getLabelString)
      Right(b)
    }
    case _ if r.isURIResource => {
      Right(IRI(r.asResource.getURI))
    }
    case lit: JenaLiteral => {
      extendNS(lit.getDatatype.getURI) match {
        case None | Some(RDFNode.`StringDatatypeIRI`) =>
          Right(StringLiteral(lit.getLexicalForm))
        case Some(RDFNode.`IntegerDatatypeIRI`) => {
          Try(IntegerLiteral(lit.getLexicalForm.toInt, lit.getLexicalForm)).fold(
            e => Right(DatatypeLiteral(lit.getLexicalForm, RDFNode.IntegerDatatypeIRI)),
            Right(_)
          )
        }
       case Some(RDFNode.`DecimalDatatypeIRI`) =>
              Try(DecimalLiteral(lit.getLexicalForm.toDouble, lit.getLexicalForm)).fold(
                e => Right(DatatypeLiteral(lit.getLexicalForm, RDFNode.DecimalDatatypeIRI)),Right(_))
       case Some(RDFNode.`DoubleDatatypeIRI`) =>
              Try(DoubleLiteral(lit.getLexicalForm.toDouble,lit.getLexicalForm)).fold(
                e => Right(DatatypeLiteral(lit.getLexicalForm, RDFNode.DoubleDatatypeIRI)),Right(_))
       case Some(RDFNode.BooleanDatatypeIRI) => {
              // Lexical form of boolean literals is lowercase true or false
              lit.getLexicalForm match {
                case "true" => Right(BooleanLiteral(true))
                case "false" => Right(BooleanLiteral(false))
                case _ => Right(DatatypeLiteral(lit.getLexicalForm, RDFNode.BooleanDatatypeIRI))
              }
            }
       case Some(RDFNode.`LangStringDatatypeIRI`) => {
              // TODO: Check that the language tag conforms to BCP 47 (https://tools.ietf.org/html/bcp47#section-2.1)
              Right(LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage)))
            }
       case Some(datatype) => {
              Right(DatatypeLiteral(lit.getLexicalForm, datatype))
            }
          }
        }
   case _ =>
    Left(s"resource2RDFNode: unexpected type of resource: $r")
  }

  def property2IRI(p: Property): IRI = IRI(p.getURI)

  def createResource(m: JenaModel, node: RDFNode, base: Option[IRI]): JenaResource = {
    node match {
      case BNode(id) => m.createResource(new AnonId(id.toString))
      case i: IRI => m.createResource(resolve(i,base))
      case _ => throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createRDFNode(m: JenaModel,
                    node: RDFNode,
                    base: Option[IRI]): JenaRDFNode = {
    val xsd = "http://www.w3.org/2001/XMLSchema#"
    val xsdinteger = xsd + "integer"
    val xsddouble = xsd + "double"
    val xsddecimal = xsd + "decimal"
    val xsdboolean = xsd + "boolean"

    node match {
      case BNode(id) =>
        m.createResource(new AnonId(id.toString))
      case i: IRI =>
        m.createResource(resolve(i,base))
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
      case l@DecimalLiteral(d, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDdecimal)
      case l@IntegerLiteral(i, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDinteger)
      case LangLiteral(l, Lang(lang)) => m.createLiteral(l, lang)
      case BooleanLiteral(b) =>
        m.createTypedLiteral(b.toString, XSDDatatype.XSDboolean)
      case l@DoubleLiteral(d: Double, repr) =>
        m.createTypedLiteral(l.lexicalForm, XSDDatatype.XSDdouble)
      case _ =>
        throw new Exception("Cannot create a resource from " + node)
    }
  }

  def createProperty(m: JenaModel, pred: IRI, base: Option[IRI]): Property = {
    m.createProperty(resolve(pred,base))
  }

  def triplesSubject(resource: JenaResource, model: JenaModel): Set[Statement] = {
    model.listStatements(resource, null, null).toSet.asScala.toSet
  }

  def triplesSubjectPredicate(resource: JenaResource,
                              pred: IRI,
                              model: JenaModel,
                              base: Option[IRI]
                             ): Set[Statement] = {
    model.listStatements(resource, createProperty(model,pred,base), null).toSet.asScala.toSet
  }

  def triplesPredicateObject(pred: IRI,
                             resource: JenaResource,
                             model: JenaModel,
                             base: Option[IRI]
                            ): Set[Statement] = {
    model.listStatements(null, createProperty(model,pred,base), resource).toSet.asScala.toSet
  }

  def triplesPredicate(pred: Property, model: JenaModel): Set[Statement] = {
    model.listStatements(null, pred, null).toSet.asScala.toSet
  }

  def triplesObject(obj: JenaResource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, null, obj).toSet.asScala.toSet
  }

  def triplesPredicateObject(property: Property, obj: JenaResource, model: JenaModel): Set[Statement] = {
    model.listStatements(null, property, obj).toSet.asScala.toSet
  }

  // TODO: Return Either[String,Path]
  def path2JenaPath(path: SHACLPath, model: JenaModel, base: Option[IRI]): Path = {
    path match {
      case PredicatePath(iri) => {
        val prop = rdfNode2Property(iri, model,base)
        new P_Link(prop.asNode)
      }
      case InversePath(path) => {
        val jenaPath = path2JenaPath(path, model,base)
        new P_Inverse(jenaPath)
      }
      case SequencePath(paths) => {
        val jenaPaths = paths.map(path => path2JenaPath(path, model,base))
        def seq(p1: Path, p2: Path): Path = new P_Seq(p1, p2)
        jenaPaths.reduce(seq)
      }
      case AlternativePath(paths) => {
        val jenaPaths = paths.map(path => path2JenaPath(path, model,base))
        def alt(p1: Path, p2: Path): Path = new P_Alt(p1, p2)
        jenaPaths.reduce(alt)
      }
      case ZeroOrMorePath(path) => {
        val jenaPath = path2JenaPath(path, model,base)
        new P_ZeroOrMoreN(jenaPath)
      }
      case ZeroOrOnePath(path) => {
        val jenaPath = path2JenaPath(path, model,base)
        new P_ZeroOrOne(jenaPath)
      }
      case OneOrMorePath(path) => {
        val jenaPath = path2JenaPath(path, model,base)
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
            Right(iri == expectedDatatype.str)
          }
          case Failure(e) => Left(e.getMessage)
        }
      }
      case _ => Right(false)
    }
  }

}
