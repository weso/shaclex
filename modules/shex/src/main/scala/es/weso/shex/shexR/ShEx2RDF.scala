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
import es.weso.rdf.nodes.{IRI, RDFNode}
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
    val sm = s.shapesMap.map{ case (l,e) => labeledShapeExpr(l,e) }.toList
    for {
      node <- makeId(id)
      _ <- addPrefixMap(s.prefixMap)
      _ <- addTriple(node,rdf_type,sx_Schema)
      shapes <- sm.sequence
    } yield ()
  }

  def labeledShapeExpr(lbl: ShapeLabel, e: ShapeExpr): RDFSaver[RDFNode] = {
    val node : RDFNode = lbl match {
      case IRILabel(iri) => iri
      case BNodeLabel(bnode) => bnode
    }
    for {
      _ <- shapeExpr(node,e)
    } yield node
  }

  def shapeExpr(node: RDFNode, e: ShapeExpr): RDFSaver[Unit] = e match {
    case ShapeExternal() => addTriple(node, rdf_type, sx_ShapeExternal)
    case NodeConstraint(nk,dt,facets,values) => for {
      _ <- maybeAddContent(nk, node, sx_nodeKind, nodeKind)
    } yield ()
  }

  def maybeAddContent[A](maybe: Option[A], node: RDFNode, pred: IRI, saver: (RDFNode, IRI, A) => RDFSaver[Unit]): RDFSaver[Unit] = {
    maybe match {
      case None => State.pure(())
      case Some(x) => saver(node,pred, x)
    }
  }

  def nodeKind(node: RDFNode, pred: IRI, nk: NodeKind): RDFSaver[Unit] =
    nk match {
      case IRIKind => addTriple(node,pred,sx_iri)
      case BNodeKind => addTriple(node,pred,sx_bnode)
      case LiteralKind => addTriple(node,pred,sx_literal)
      case NonLiteralKind => addTriple(node,pred,sx_nonliteral)
    }


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