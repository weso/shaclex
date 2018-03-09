package es.weso.rdf.rdf4j

import es.weso.rdf.nodes._
import es.weso.rdf.triples._

import scala.collection.JavaConverters._
import org.eclipse.rdf4j.model.{IRI => IRI_RDF4j, Literal => Literal_RDF4j, _}
import org.eclipse.rdf4j.model.impl.{SimpleBNode, SimpleIRI, SimpleLiteral, SimpleValueFactory, BooleanLiteral => BooleanLiteral_RDF4j, DecimalLiteral => DecimalLiteral_RDF4j, IntegerLiteral => IntegerLiteral_RDF4j}
import org.eclipse.rdf4j.model.util.Models

object RDF4jMapper {

  lazy val valueFactory = SimpleValueFactory.getInstance;

  private[rdf4j] def literal2Literal(lit: Literal_RDF4j): Literal = {
    lit match {
      case bl: BooleanLiteral_RDF4j => BooleanLiteral(bl.booleanValue())
      case il: IntegerLiteral_RDF4j => IntegerLiteral(il.intValue())
      case dl: DecimalLiteral_RDF4j => DecimalLiteral(dl.decimalValue())
      case _ => DatatypeLiteral(lit.stringValue(),iri2iri(lit.getDatatype))
    }
  }

  private[rdf4j] def iri2iri(iri: IRI_RDF4j): IRI = IRI(iri.toString)

  private[rdf4j] def bnode2Bnode(b: BNode): BNodeId = BNodeId(b.getID)

  private[rdf4j] def value2RDFNode(value: Value): RDFNode = {
    value match {
      case lit: Literal_RDF4j => literal2Literal(lit)
      case bnode: BNode => bnode2Bnode(bnode)
      case iri: IRI_RDF4j => iri2iri(iri)
    }
  }

  private[rdf4j] def statement2RDFTriple(s: Statement): RDFTriple = {
    RDFTriple(resource2RDFNode(s.getSubject), iri2iri(s.getPredicate), value2RDFNode(s.getObject))
  }

  private[rdf4j] def resource2RDFNode(r: Resource): RDFNode = {
    r match {
      case iri: IRI_RDF4j => iri2iri(iri)
      case bnode: BNode => bnode2Bnode(bnode)
      case lit: Literal_RDF4j => literal2Literal(lit)
    }
  }

  private[rdf4j] def iri2Property(iri: IRI): IRI_RDF4j = {
    valueFactory.createIRI(iri.str)
  }

  private[rdf4j] def rdfNode2Resource(r: RDFNode): Either[String,Resource] = {
    r match {
      case iri: IRI => Right(valueFactory.createIRI(iri.str))
      case bnode: BNodeId => Right(valueFactory.createBNode(bnode.id))
      case _ => Left(s"Cannot convert rdfNode: $r to Resource")
    }
  }


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
}