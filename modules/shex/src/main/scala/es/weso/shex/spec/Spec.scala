package es.weso.shex.spec

import cats._
import cats.implicits._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, Literal, RDFNode}
import es.weso.shapeMaps.{FixedShapeMap, ShapeMapLabel}
import es.weso.shex._
import es.weso.shex.btValidator.ShExErr
import es.weso.typing.Typing

object Spec {

  type ShapeTyping = Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]]

  def satisfies(n: RDFNode, se: ShapeExpr, rdf: RDFReader, m: FixedShapeMap): Either[String,Boolean] = se match {
    case nc: NodeConstraint => satisfies2(n, nc)
    case ShapeAnd(id,ses) => satisfyAll(ses.map(satisfies(n,_,rdf,m)))
    case _ => unimplemented(s"Satisfies on $se")
  }

  def satisfies2(n: RDFNode, nc: NodeConstraint): Either[String,Boolean] = nodeSatisfies(n,nc)

  def nodeSatisfies(n: RDFNode, nc: NodeConstraint): Either[String,Boolean] = for {
    nk <- optSatisfy(nc.nodeKind, satisfiesNodeKind(n))
    dt <- optSatisfy(nc.datatype, satisfiesDatatype(n))
  } yield List(nk,dt).forall(_ == true)

  def satisfiesNodeKind(n: RDFNode)(nk: NodeKind) = nk match {
    case IRIKind => Right(n.isIRI)
    case BNodeKind => Right(n.isBNode)
    case LiteralKind => Right(n.isLiteral)
    case NonLiteralKind => Right(n.isNonLiteral)
  }
  def satisfiesDatatype(n: RDFNode)(dt: IRI) = n match {
    case l: Literal => Right(l.dataType == dt)
    case _ => Right(false)
  }


  def optSatisfy[A](maybeA: Option[A], check: A => Either[String,Boolean]): Either[String,Boolean] = maybeA match {
    case None => Right(true)
    case Some(a) => check(a)
  }

  def satisfyAll(ls: List[Either[String,Boolean]]): Either[String,Boolean] =
    ls.sequence.map(_.forall(_ == true))

  def unimplemented(msg: String) = Left(s"Unimplemented: $msg")

}