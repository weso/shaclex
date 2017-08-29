package es.weso.shex.implicits

import cats._
import data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.rdf._

object eqShEx extends LazyLogging {

  implicit lazy val eqIRI = new Eq[IRI] {
    final def eqv(n1: IRI, n2: IRI): Boolean = (n1, n2) match {
      case (IRI(i1), IRI(i2)) => i1 == i2
    }
  }

  implicit lazy val eqSchema: Eq[Schema] = new Eq[Schema] {
    final def eqv(s1: Schema, s2: Schema): Boolean = {
      s1.prefixes === s2.prefixes &&
        s1.base === s2.base &&
        s1.startActs === s2.startActs &&
        s1.start === s2.start &&
        s1.shapes === s2.shapes
    }
  }

  implicit lazy val eqPrefixMap: Eq[PrefixMap] = new Eq[PrefixMap] {
    final def eqv(pm1: PrefixMap, pm2: PrefixMap): Boolean = {
      pm1.pm === pm2.pm
    }
  }

  implicit lazy val eqMax: Eq[Max] = new Eq[Max] {
    final def eqv(a1: Max, a2: Max): Boolean = (a1, a2) match {
      case (Star, Star) => true
      case (IntMax(m), IntMax(n)) => m === n
      case (_, _) => false
    }
  }

  implicit lazy val eqShapeLabel: Eq[ShapeLabel] = new Eq[ShapeLabel] {
    final def eqv(a1: ShapeLabel, a2: ShapeLabel): Boolean = (a1, a2) match {
      case (IRILabel(b1), IRILabel(b2)) => b1 === b2
      case (BNodeLabel(b1), BNodeLabel(b2)) => b1 == b2
      case (_, _) => false
    }
  }

  implicit lazy val eqSemAct: Eq[SemAct] = new Eq[SemAct] {
    final def eqv(a1: SemAct, a2: SemAct): Boolean = a1 == a2
  }

  implicit lazy val eqShapeExpr: Eq[ShapeExpr] = new Eq[ShapeExpr] {
    final def eqv(a1: ShapeExpr, a2: ShapeExpr): Boolean = (a1, a2) match {
      case (ShapeOr(id1, ss1), ShapeOr(id2, ss2)) => id1 == id2 && ss1 === ss2
      case (ShapeAnd(id1, ss1), ShapeAnd(id2, ss2)) => id1 == id2 && ss1 === ss2
      case (ShapeNot(id1, s1), ShapeNot(id2, s2)) => id1 == id2 && s1 === s2
      case (n1: NodeConstraint, n2: NodeConstraint) => n1 === n2
      case (s1: Shape, s2: Shape) => s1 === s2
      case (ShapeRef(i1), ShapeRef(i2)) => i1 === i2
      case (ShapeExternal(id1), ShapeExternal(id2)) => id1 == id2
      case (_, _) => false
    }
  }

  implicit lazy val eqShape: Eq[Shape] = new Eq[Shape] {
    final def eqv(s1: Shape, s2: Shape): Boolean =
      s1.isVirtual === s2.isVirtual &&
        s1.isClosed === s2.isClosed &&
        s1.expression === s2.expression &&
        s1.inherit === s2.inherit &&
        s1.semActs === s2.semActs
  }

  implicit lazy val eqTripleExpr: Eq[TripleExpr] = new Eq[TripleExpr] {
    final def eqv(s1: TripleExpr, s2: TripleExpr): Boolean = (s1, s2) match {
      case (e1: EachOf, e2: EachOf) => e1 === e2
      case (e1: OneOf, e2: OneOf) => e1 === e2
      case (Inclusion(l1), Inclusion(l2)) => l1 === l2
      case (t1: TripleConstraint, t2: TripleConstraint) => t1 === t2
      case (_, _) => false
    }
  }

  implicit lazy val eqEachOf: Eq[EachOf] = new Eq[EachOf] {
    final def eqv(s1: EachOf, s2: EachOf): Boolean =
      s1.expressions === s2.expressions &&
        s1.min === s2.min &&
        s1.max === s2.max &&
        s1.semActs === s2.semActs &&
        s1.annotations === s2.annotations
  }

  implicit lazy val eqSomeOf: Eq[OneOf] = new Eq[OneOf] {
    final def eqv(s1: OneOf, s2: OneOf): Boolean =
      s1.expressions === s2.expressions &&
        s1.min === s2.min &&
        s1.max === s2.max &&
        s1.semActs === s2.semActs &&
        s1.annotations === s2.annotations
  }

  implicit lazy val eqTripleConstraint: Eq[TripleConstraint] = new Eq[TripleConstraint] {
    final def eqv(s1: TripleConstraint, s2: TripleConstraint): Boolean = {
      // println(s"Comparing triple constraints $s1 and $s2")
      // println(s"Min/max (${s1.min}/${s1.max}) and (${s2.min}/${s2.max})")

      s1.inverse === s2.inverse &&
        s1.negated === s2.negated &&
        s1.predicate === s2.predicate &&
        s1.min === s2.min &&
        s1.max === s2.max &&
        s1.semActs === s2.semActs &&
        s1.annotations === s2.annotations
    }
  }

  implicit lazy val eqAnnotation: Eq[Annotation] = new Eq[Annotation] {
    final def eqv(s1: Annotation, s2: Annotation): Boolean =
      s1.predicate === s2.predicate &&
        s1.obj == s2.obj // TODO?
  }

  implicit lazy val eqNodeKind = new Eq[NodeKind] {
    final def eqv(n1: NodeKind, n2: NodeKind): Boolean = (n1, n2) match {
      case (IRIKind, IRIKind) => true
      case (BNodeKind, BNodeKind) => true
      case (NonLiteralKind, NonLiteralKind) => true
      case (LiteralKind, LiteralKind) => true
      case (_, _) => false
    }
  }

  implicit lazy val eqNodeConstraint: Eq[NodeConstraint] = new Eq[NodeConstraint] {

    final def eqv(n1: NodeConstraint, n2: NodeConstraint): Boolean = {
      // logger.info(s"Testing eq on nodeConstraints\n$n1\n$n2")
      n1.nodeKind === n2.nodeKind &&
        n1.datatype === n2.datatype &&
        n1.xsFacets.toSet === n2.xsFacets.toSet &&
        n1.values.getOrElse(List()).toSet ===
        n2.values.getOrElse(List()).toSet
    }
  }

}
