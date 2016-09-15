package es.weso.shex.converter

import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.implicits._
import es.weso.converter._

object ShEx2Shacl extends Converter {

  def shex2Shacl(schema: shex.Schema): Result[shacl.Schema] =
    getShaclShapes(schema).map(shapes => shacl.Schema(shapes))

  def getShaclShapes(schema: shex.Schema): Result[Seq[shacl.Shape]] = {
    val shapesMap: Map[shex.ShapeLabel,shex.ShapeExpr] = schema.shapes.getOrElse(Map())
    val zero: Result[Seq[shacl.Shape]] = ok(Seq())
    def comb(
      rs: Result[Seq[shacl.Shape]],
      pair: (shex.ShapeLabel,shex.ShapeExpr)): Result[Seq[shacl.Shape]] = {
      val r1: Result[IRI] = schema.resolveShapeLabel(pair._1).toValidatedNel
      val r2: Result[shacl.Shape] = mkShape(pair._2, schema)
      val r : Result[Shape] = (r1 |@| r2).map((iri,shape) => shape.addId(iri))
      r.product(rs).map{ case (x,xs) => x +: xs }
    }
    shapesMap.foldLeft(zero)(comb)
  }

  def mkShape(shape: shex.ShapeExpr, schema: shex.Schema): Result[shacl.Shape] =
    shape match {
      case s: shex.ShapeAnd => err(s"mkShape: Not implemented $s")
      case s: shex.ShapeOr => err(s"mkShape: Not implemented $s")
      case s: shex.ShapeNot => err(s"mkShape: Not implemented $s")
      case nc: shex.NodeConstraint => nodeConstraint2Shape(nc, schema)
      case s: shex.Shape => err(s"mkShape: Not implemented $s")
      case s: shex.ShapeRef => err(s"mkShape: Not implemented $s")
      case s: shex.ShapeExternal => err(s"mkShape: Not implemented $s")
    }

  def nodeConstraint2Shape(nc: shex.NodeConstraint, schema: shex.Schema): Result[shacl.Shape] = {
    val nkShape: Result[List[Component]] =
      nc.nodeKind.map(cnvNodeKind(_)).sequence.map(_.toList)
    nkShape.map(nks =>
       shacl.Shape.empty.copy(
         constraints = Seq(NodeConstraint(nks))))
  }

  def cnvNodeKind(nk: shex.NodeKind): Result[shacl.NodeKind] =
    nk match {
      case shex.IRIKind => ok(shacl.NodeKind(shacl.IRIKind))
      case shex.BNodeKind => ok(shacl.NodeKind(shacl.BlankNodeKind))
      case shex.LiteralKind => ok(shacl.NodeKind(shacl.LiteralKind))
      case shex.NonLiteralKind => ok(shacl.NodeKind(shacl.BlankNodeOrIRI))
    }
}
