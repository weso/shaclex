package es.weso.rdf.rdf4j

import es.weso.rdf.nodes._
import es.weso.rdf.triples._
import scala.collection.JavaConverters._
import org.eclipse.rdf4j.model.{IRI => IRI_RDF4j, Literal => Literal_RDF4j, BNode => BNode_RDF4j, _}
import org.eclipse.rdf4j.model.impl.{SimpleValueFactory, BooleanLiteral => BooleanLiteral_RDF4j, DecimalLiteral => DecimalLiteral_RDF4j, IntegerLiteral => IntegerLiteral_RDF4j}
import cats.implicits._
import org.eclipse.rdf4j.model.vocabulary.XMLSchema

object RDF4jMapper {

  lazy val valueFactory = SimpleValueFactory.getInstance;

  private[rdf4j] def literal2Literal(lit: Literal_RDF4j): Literal = {
    lit match {
      case bl: BooleanLiteral_RDF4j => BooleanLiteral(bl.booleanValue())
      case il: IntegerLiteral_RDF4j => IntegerLiteral(il.intValue())
      case dl: DecimalLiteral_RDF4j => DecimalLiteral(dl.decimalValue())
      case _ if (lit.getDatatype == XMLSchema.STRING) => StringLiteral(lit.stringValue())
      case _ if (lit.getLanguage.isPresent) => LangLiteral(lit.stringValue, Lang(lit.getLanguage.get()))
      case _ => DatatypeLiteral(lit.stringValue(), iri2iri(lit.getDatatype))
    }
  }

  private[rdf4j] def iri2iri(iri: IRI_RDF4j): IRI = IRI(iri.toString)

  private[rdf4j] def bnode2Bnode(b: BNode_RDF4j): BNode = BNode(b.getID)

  private[rdf4j] def value2RDFNode(value: Value): RDFNode = {
    value match {
      case lit: Literal_RDF4j => literal2Literal(lit)
      case bnode: BNode_RDF4j => bnode2Bnode(bnode)
      case iri: IRI_RDF4j => iri2iri(iri)
    }
  }

  private[rdf4j] def statement2RDFTriple(s: Statement): RDFTriple = {
    RDFTriple(resource2RDFNode(s.getSubject), iri2iri(s.getPredicate), value2RDFNode(s.getObject))
  }

  private[rdf4j] def resource2RDFNode(r: Resource): RDFNode = {
    r match {
      case iri: IRI_RDF4j => iri2iri(iri)
      case bnode: BNode_RDF4j => bnode2Bnode(bnode)
      case lit: Literal_RDF4j => literal2Literal(lit)
    }
  }

  private[rdf4j] def iri2Property(iri: IRI): IRI_RDF4j = {
    valueFactory.createIRI(iri.str)
  }

  private[rdf4j] def rdfNode2Resource(r: RDFNode): Either[String, Resource] = {
    r match {
      case iri: IRI => Right(valueFactory.createIRI(iri.str))
      case bnode: BNode => Right(valueFactory.createBNode(bnode.id))
      case _ => Left(s"Cannot convert rdfNode: $r to Resource")
    }
  }

 private[rdf4j] def rdfNode2Value(r: RDFNode): Value = r match {
   case iri: IRI => iri2Property(iri)
   case bnode: BNode => valueFactory.createBNode(bnode.id)
   case StringLiteral(str) => valueFactory.createLiteral(str)
   case BooleanLiteral(b) => valueFactory.createLiteral(b)
   case IntegerLiteral(i) => valueFactory.createLiteral(i)
   case DecimalLiteral(d) => valueFactory.createLiteral(d.bigDecimal)
   case DoubleLiteral(d) => valueFactory.createLiteral(d)
   case DatatypeLiteral(l,d) => valueFactory.createLiteral(l,iri2Property((d)))
   case LangLiteral(l,Lang(lang)) => valueFactory.createLiteral(l,lang)
 }

 private[rdf4j] def newBNode(): BNode_RDF4j = valueFactory.createBNode()

 private[rdf4j] def statements2RDFTriples(statements: Set[Statement]): Set[RDFTriple] = {
    statements.map(statement2RDFTriple(_))
  }

  private[rdf4j] def triplesSubject(resource: Resource, model: Model): Set[Statement] = {
    model.filter(resource, null, null).asScala.toSet
  }

  private[rdf4j] def triplesPredicate(iri: IRI_RDF4j, model: Model): Set[Statement] = {
    model.filter(null, iri, null).asScala.toSet
  }

  private[rdf4j] def triplesObject(value: Value, model: Model): Set[Statement] = {
    model.filter(null, null, value).asScala.toSet
  }

  private[rdf4j] def triplesPredicateObject(iri: IRI_RDF4j, obj: Value, model: Model): Set[Statement] = {
    model.filter(null, iri, obj).asScala.toSet
  }

  private[rdf4j] def rdfTriples2Model(triples: Set[RDFTriple]): Either[String, Model] = for {
    ss <- triples.map(rdfTriple2Statement(_)).toList.sequence
  } yield {
    
    ???
  }

  private[rdf4j] def rdfTriple2Statement(triple: RDFTriple): Either[String, Statement] = {
    val pred = iri2Property(triple.pred)
    val obj = rdfNode2Value(triple.obj)
    for {
      subj <- rdfNode2Resource(triple.subj)
    } yield valueFactory.createStatement(subj, pred, obj)
  }

  // TODO: Check rules of datatype
  private[rdf4j] def wellTypedDatatype(node: RDFNode, expectedDatatype: IRI): Either[String,Boolean] = node match {
    case DatatypeLiteral(_,dt) => Right(dt == expectedDatatype)
    case _ => Right(false)
  }

}