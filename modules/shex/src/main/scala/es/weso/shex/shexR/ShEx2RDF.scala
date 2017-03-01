package es.weso.shex.shexR

import es.weso.rdf.RDFBuilder
import es.weso.shex._
import PREFIXES._
import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES.{rdf, rdfs, xsd}
import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import org.apache.jena.rdf.model.Model

import scala.util.{Success, Try}

trait ShEx2RDF extends RDFSaver with LazyLogging {

  def serialize(shex:Schema, node: Option[IRI], format: String): Try[String] = {
    val rdf: RDFAsJenaModel = toRDF(shex,node,RDFAsJenaModel.empty)
    Success(rdf.serialize(format))
  }

  def toRDF(s: Schema, node: Option[IRI], initial: RDFAsJenaModel): RDFAsJenaModel = {
    val result = schema(s,node).run(initial)
    result.value._1
  }

  def schema(s:Schema, id: Option[IRI]): RDFSaver[Unit] = {
    for {
      node <- makeId(id)
      _ <- addPrefixMap(s.prefixMap)
      _ <- addTriple(node,rdf_type,sx_Schema)
      _ <- maybeAddListContent(s.shapes, node, sx_shapes, shapeExpr)
    } yield ()
  }

  def listSaver[A](ls: List[A], saver: A => RDFSaver[RDFNode]): RDFSaver[List[RDFNode]] = {
    ls.map(saver(_)).sequence
  }

  def addListContent[A](ls: List[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    nodes <- listSaver(ls,saver)
    _ <- nodes.map(n => addTriple(node, pred, n)).sequence
  } yield ()

  def maybeAddListContent[A](maybeLs: Option[List[A]], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] =
   maybeLs match {
     case None => ok(())
     case Some(ls) => addListContent(ls,node,pred,saver)
  }

  def mkId(id: Option[ShapeLabel]): RDFSaver[RDFNode] = id match {
    case None => createBNode
    case Some(IRILabel(iri)) => ok(iri)
    case Some(BNodeLabel(bNode)) => ok(bNode)
  }

  def shapeExpr(e: ShapeExpr): RDFSaver[RDFNode] = e match {
    case ShapeAnd(id,shapeExprs) => for {
      node <- mkId(id)
      _ <- addTriple(node,rdf_type,sx_ShapeAnd)
      _ <- addListContent(shapeExprs,node,sx_expressions,shapeExpr)
    } yield node
    case ShapeOr(id,shapeExprs) => for {
      node <- mkId(id)
      _ <- addTriple(node,rdf_type,sx_ShapeOr)
      _ <- addListContent(shapeExprs,node,sx_expressions,shapeExpr)
    } yield node
    case ShapeNot(id,se) => for {
      node <- mkId(id)
      _ <- addTriple(node,rdf_type,sx_ShapeNot)
      _ <- addContent(se,node,sx_expression,shapeExpr)
    } yield node
    case Shape(id,virtual,closed,extra,expr,inherit,semActs) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId,rdf_type,sx_Shape)
      _ <- maybeAddContent(closed,shapeId,sx_closed, rdfBoolean)
      _ <- maybeAddContent(expr, shapeId, sx_expression, tripleExpr)
    } yield shapeId
    case ShapeExternal(id) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId, rdf_type, sx_ShapeExternal)
    } yield shapeId
    case NodeConstraint(id,nk,dt,facets,values) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId,rdf_type,sx_NodeConstraint)
      _ <- maybeAddContent(nk, shapeId, sx_nodeKind, nodeKind)
      _ <- maybeAddContent(dt, shapeId, sx_datatype, iri)
//TODO      _ <- addListContent(facets, shapeId, sx_, datatype)
//TODO      _ <- maybeAddListContent(values,shapeId,sx_values,valueSetValue)
    } yield shapeId
    case ShapeRef(lbl) => label(lbl)
  }

  def optSaver[A](maybe: Option[A], saver: A => RDFSaver[RDFNode]): RDFSaver[Option[RDFNode]] = {
    maybe match {
      case None => ok(None)
      case Some(x) => saver(x).map(Some(_))
    }
  }

  def maybeAddTriple[A](node: RDFNode, pred: IRI, maybe: Option[RDFNode]): RDFSaver[Unit] = {
    maybe match {
      case None => State.pure(())
      case Some(x) => addTriple(node,pred, x)
    }
  }

  def addContent[A](x: A, node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    n <- saver(x)
    _ <- addTriple(node, pred, n)
  } yield ()

  def maybeAddContent[A](maybe: Option[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    maybeNode <- optSaver(maybe, saver)
    _ <- maybeAddTriple(node, pred, maybeNode)
  } yield ()

  def rdfBoolean(x: Boolean): RDFSaver[RDFNode] = x match {
    case true => ok(BooleanLiteral(true))
    case false => ok(BooleanLiteral(false))
  }

  def rdfInt(x: Int): RDFSaver[RDFNode] =
    ok(IntegerLiteral(x))

  def rdfMax(x: Max): RDFSaver[RDFNode] = x match {
    case IntMax(n) => rdfInt(n)
    case Star => ok(sx_INF)
  }

  def tripleExpr(te: TripleExpr): RDFSaver[RDFNode] = te match {
    case TripleConstraint(id,inverse,negated,pred,valueExpr,min,max,semActs,annotations) => for {
      teId <- mkId(id)
      _ <- addTriple(teId,rdf_type,sx_TripleConstraint)
      _ <- maybeAddContent(inverse,teId,sx_inverse,rdfBoolean)
      _ <- maybeAddContent(negated,teId,sx_negated,rdfBoolean)
      _ <- addTriple(teId,sx_predicate,pred)
      _ <- maybeAddContent(valueExpr,teId,sx_valueExpr,shapeExpr)
      _ <- maybeAddContent(min,teId,sx_min,rdfInt)
      _ <- maybeAddContent(max,teId,sx_max,rdfMax)
      _ <- maybeAddListContent(semActs,teId,sx_semActs,semAct)
      _ <- maybeAddListContent(annotations,teId,sx_annotation, annotation)
    } yield teId
    case EachOf(id,exprs,min,max,semActs,annotations) => for {
      node <- mkId(id)
      _ <- addTriple(node,rdf_type,sx_EachOf)
      _ <- addListContent(exprs, node,sx_expressions,tripleExpr)
      _ <- maybeAddContent(min,node,sx_min,rdfInt)
      _ <- maybeAddContent(max,node,sx_max,rdfMax)
      _ <- maybeAddListContent(semActs,node,sx_semActs,semAct)
      _ <- maybeAddListContent(annotations,node,sx_annotation, annotation)
    } yield node
    case OneOf(id,exprs,min,max,semActs,annotations) => for {
      node <- mkId(id)
      _ <- addTriple(node,rdf_type,sx_OneOf)
      _ <- addListContent(exprs, node,sx_expressions,tripleExpr)
      _ <- maybeAddContent(min,node,sx_min,rdfInt)
      _ <- maybeAddContent(max,node,sx_max,rdfMax)
      _ <- maybeAddListContent(semActs,node,sx_semActs,semAct)
      _ <- maybeAddListContent(annotations,node,sx_annotation, annotation)
    } yield node
    case Inclusion(lbl) => label(lbl)
  }

  def semAct(x: SemAct): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id,rdf_type,sx_SemAct)
    _ <- addTriple(id,sx_name,x.name)
    _ <- maybeAddContent(x.code,id,sx_code,rdfString)
  } yield id

  def rdfString(x: String): RDFSaver[RDFNode] =
    ok(StringLiteral(x))

  def annotation(x: Annotation): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id,rdf_type,sx_Annotation)
    _ <- addTriple(id,sx_predicate,x.predicate)
    _ <- addContent(x.obj,id,sx_object,objectValue)
  } yield id

  def objectValue(x: ObjectValue): RDFSaver[RDFNode] = x match {
    case IRIValue(iri) => ok(iri)
    case StringValue(s) => ok(StringLiteral(s))
    case DatatypeString(s,iri) => ok(DatatypeLiteral(s,iri))
    case LangString(s,lang) => ok(LangLiteral(s,Lang(lang)))
  }

  def label(lbl: ShapeLabel): RDFSaver[RDFNode] = lbl match {
    case IRILabel(iri) => ok(iri)
    case BNodeLabel(bnode) => ok(bnode)
  }

  def nodeKind(nk: NodeKind): RDFSaver[RDFNode] =
    nk match {
      case IRIKind => ok(sx_iri)
      case BNodeKind => ok(sx_bnode)
      case LiteralKind => ok(sx_literal)
      case NonLiteralKind => ok(sx_nonliteral)
    }

  def iri(i: IRI): RDFSaver[RDFNode] = ok(i)


  def addPrefix(alias: String, iri: IRI): RDFSaver[Unit] = {
      State.modify(_.addPrefix(alias,iri.str))
  }
}

object ShEx2RDF {

  def shEx2Model(s: Schema, n: Option[IRI]): Model = {
    val srdf = new ShEx2RDF {}
    srdf.toRDF(s,n,RDFAsJenaModel.empty).model
  }

}