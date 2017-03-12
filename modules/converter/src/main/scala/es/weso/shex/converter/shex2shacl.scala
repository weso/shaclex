package es.weso.shex.converter

import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import es.weso.rdf.PrefixMap
import cats._
import cats.data._
import cats.implicits._
import es.weso.converter._
import es.weso.rdf._

object ShEx2Shacl extends Converter {

  def shex2Shacl(schema: shex.Schema): Result[shacl.Schema] =
    getShaclShapes(schema).map(
      shapes => shacl.Schema(
        pm = schema.prefixMap,
        shapes = shapes
      )
    )

  def cnvPrefixMap(pm: PrefixMap): Map[String,IRI] = {
    pm.pm.map{ case (prefix,value) => (prefix.str,value) }
  }

  def getShaclShapes(schema: shex.Schema): Result[Seq[shacl.Shape]] = {
    val shapesMap: List[shex.ShapeExpr] = schema.shapes.getOrElse(List())
    val zero: Result[Seq[shacl.Shape]] = ok(Seq())
    def comb(rs: Result[Seq[shacl.Shape]],
             shapeExpr: shex.ShapeExpr): Result[Seq[shacl.Shape]] = {
      val r1: Result[IRI] = shapeExpr.id match {
        case None => err(s"shapeExpr $shapeExpr doesn't have id: Not implemented resolution of this case yet")
        case Some(lbl) => schema.resolveShapeLabel(lbl).toValidatedNel
      }
      val r2: Result[shacl.Shape] = cnvShapeExpr(shapeExpr, schema)
      val r : Result[Shape] = (r1 |@| r2).map((iri, shape) => shape.addId(iri))
      r.product(rs).map{ case (x,xs) => x +: xs }
    }
    shapesMap.foldLeft(zero)(comb)
  }

  def cnvShapeExpr(se: shex.ShapeExpr, schema: shex.Schema): Result[shacl.Shape] =
    se match {
      case s: shex.ShapeAnd => ??? // cnvShapeAnd(s,schema)
      case s: shex.ShapeOr => ??? // cnvShapeOr(s,schema)
      case s: shex.ShapeNot => ??? // cnvShapeNot(s,schema)
      case nc: shex.NodeConstraint => cnvNodeConstraint(nc, schema)
      case s: shex.Shape => cnvShape(s,schema)
      case s: shex.ShapeRef => err(s"mkShape: Not implemented $s")
      case s: shex.ShapeExternal => err(s"mkShape: Not implemented $s")
    }

/*  def cnvShapeAnd(shapeAnd: shex.ShapeAnd, schema: shex.Schema): Result[shacl.Shape] = {
    shapeAnd.shapeExprs.
         map(cnvShapeExpr(_,schema)).
         sequence.
         map(shapes => shacl.Shape.empty.copy(
           constraints =
             Seq(NodeShape(None, List(And(shapes)))
            )))
  }

  def cnvShapeOr(shapeOr: shex.ShapeOr, schema: shex.Schema): Result[shacl.Shape] = {
    shapeOr.shapeExprs.
         map(cnvShapeExpr(_,schema)).
         sequence.
         map(shapes => shacl.Shape.empty.copy(
           constraints =
             Seq(NodeShape(None, List(Or(shapes)))
            )))
  }

  def cnvShapeNot(shapeNot: shex.ShapeNot, schema: shex.Schema): Result[shacl.Shape] = {
    cnvShapeExpr(shapeNot.shapeExpr,schema).
       map(shape => shacl.Shape.empty.copy(
          constraints =
            Seq(NodeShape(None, List(Not(shape)))
          )))
  }
*/

  def cnvShape(shape: shex.Shape, schema: shex.Schema): Result[shacl.Shape] = {
    // TODO: Error handling when virtual, inherit, extra, semActs are defined...
    shape.expression match {
     case None => err(s"No expression in shape $shape")
     case Some(te) => cnvTripleExpr(te,schema)
    }
  }

  def cnvTripleExpr(te: shex.TripleExpr, schema: shex.Schema): Result[shacl.Shape] = {
    te match {
     case e: shex.EachOf => err(s"cnvTripleExpr: Not implemented EachOf conversion")
     case e: shex.OneOf => err(s"cnvTripleExpr: Not implemented OneOf conversion")
     case e: shex.Inclusion => err(s"cnvTripleExpr: Not implemented Inclusion conversion")
     case tc: shex.TripleConstraint => cnvTripleConstraint(tc,schema)
    }
  }

  def cnvTripleConstraint(
                           tc: shex.TripleConstraint,
                           schema: shex.Schema
                         ): Result[shacl.Shape] = {
    if (tc.negated)
      err(s"cnvTripleConstraint: Not implemented negated")
    else {
    val path = if (!tc.inverse) PredicatePath(tc.predicate)
               else InversePath(PredicatePath(tc.predicate))
     val pc: Result[PropertyShape] =
       mkPropertyShape(path,
         tc.valueExpr, tc.min, tc.max, schema)
//     ok(Constraint.empty.copy(constraints = Seq(pc)))
    ??? // pc
   }
  }

 def mkPropertyShape(
   path: SHACLPath,
   valueExpr: Option[shex.ShapeExpr],
   min: Int,
   max: shex.Max,
   schema: shex.Schema): Result[PropertyShape] = {
   val minComponent: List[Component] =
     if (min == Shacl.defaultMin) List()
     else List(MinCount(min))
   val maxComponent: List[Component] =
     max match {
       case shex.Star => List()
       case shex.IntMax(n) => List(MaxCount(n))
     }
//     max.getOrElse(Max(1)).map
   val components = minComponent ++ maxComponent
   ok(Shape.emptyPropertyShape(path).copy(components = components))
 }

  def cnvNodeConstraint(
       nc: shex.NodeConstraint,
       schema: shex.Schema): Result[shacl.Shape] = {
    val nkShape: Result[List[Component]] =
      nc.nodeKind.map(cnvNodeKind(_)).sequence.map(_.toList)
    nkShape.map(nks => shacl.Shape.empty.copy(components = nks))
  }

  def cnvNodeKind(nk: shex.NodeKind): Result[shacl.NodeKind] =
    nk match {
      case shex.IRIKind => ok(shacl.NodeKind(shacl.IRIKind))
      case shex.BNodeKind => ok(shacl.NodeKind(shacl.BlankNodeKind))
      case shex.LiteralKind => ok(shacl.NodeKind(shacl.LiteralKind))
      case shex.NonLiteralKind => ok(shacl.NodeKind(shacl.BlankNodeOrIRI))
    }
}
