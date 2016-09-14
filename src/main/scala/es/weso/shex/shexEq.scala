package es.weso.shex

import cats._, data._
import cats.implicits._
import es.weso.rdf.nodes._

object shexEq {

  implicit lazy val eqMax: Eq[Max] = new Eq[Max] {
    final def eqv(a1: Max, a2: Max): Boolean = (a1, a2) match {
      case (Star, Star)           => true
      case (IntMax(m), IntMax(n)) => m === n
      case (_, _)                 => false
    }
  }

  implicit lazy val eqShapeLabel: Eq[ShapeLabel] = new Eq[ShapeLabel] {
    final def eqv(a1: ShapeLabel, a2: ShapeLabel): Boolean = (a1, a2) match {
      case (IRILabel(b1),IRILabel(b2)) => b1 === b2
      case (BNodeLabel(b1),BNodeLabel(b2)) => b1 == b2
      case (_,_) => false
    }
  }
  
  implicit lazy val eqSemAct: Eq[SemAct] = new Eq[SemAct] {
    final def eqv(a1: SemAct, a2: SemAct): Boolean = a1 == a2
  }
  
  implicit lazy val eqShapeExpr: Eq[ShapeExpr] = new Eq[ShapeExpr] {
    final def eqv(a1: ShapeExpr, a2: ShapeExpr): Boolean = (a1,a2) match {
      case (ShapeOr(ss1),ShapeOr(ss2)) => ss1 === ss2
      case (ShapeAnd(ss1),ShapeAnd(ss2)) => ss1 === ss2
      case (ShapeNot(s1),ShapeNot(s2)) => s1 === s2
      case (n1:NodeConstraint,n2: NodeConstraint) => n1 === n2
      case (s1:Shape,s2:Shape) => s1 === s2
      case (ShapeRef(i1),ShapeRef(i2)) => i1 === i2
      case (ShapeExternal(),ShapeExternal()) => true
      case (_,_) => false
    }
  }
  
  implicit lazy val eqShape: Eq[Shape] = new Eq[Shape] {
    final def eqv(a1: Shape, a2: Shape): Boolean = a1 == a2
  }
  
  implicit lazy val eqNodeConstraint: Eq[NodeConstraint] = new Eq[NodeConstraint] {

    final def eqv(n1: NodeConstraint, n2: NodeConstraint): Boolean = {
      println(s"Testing eq on nodeConstraints\n$n1\n$n2")
      n1.nodeKind === n2.nodeKind &&
        n1.datatype === n2.datatype &&
        n1.xsFacets.toSet === n2.xsFacets.toSet &&
        n1.values.getOrElse(List()).toSet === n2.values.getOrElse(List()).toSet
    }
  }

  implicit lazy val eqNodeKind = new Eq[NodeKind] {
    final def eqv(n1: NodeKind, n2: NodeKind): Boolean = (n1, n2) match {
      case (IRIKind, IRIKind)               => true
      case (BNodeKind, BNodeKind)           => true
      case (NonLiteralKind, NonLiteralKind) => true
      case (LiteralKind, LiteralKind)       => true
      case (_, _)                           => false
    }
  }

  implicit lazy val eqIRI = new Eq[IRI] {
    final def eqv(n1: IRI, n2: IRI): Boolean = (n1, n2) match {
      case (IRI(i1), IRI(i2)) => i1 == i2
    }
  }

}