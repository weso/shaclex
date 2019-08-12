package es.weso.shex.compact
import cats._
import cats.implicits._
import es.weso.shex._
import es.weso.shex.implicits.eqShEx._

object CompareSchemas {
  type ShapeMap = Map[ShapeLabel, ShapeExpr]
  // Compare schemas ignoring namespaces or other minor differences like None vs Some(false)
  def compareSchemas(s1: Schema, s2: Schema): Boolean = {
    compareShapesMaps(s1.localShapesMap, s2.localShapesMap)
  }

  def compareShapesMaps(s1: ShapeMap, s2: ShapeMap): Boolean = {
    if (s1.keys == s2.keys) {
      s1.keys.foldLeft(true)((b, lbl) => {
        val maybeSes = for {
          se1 <- s1.get(lbl).toRight(s"Error obtaining label $lbl in $s1")
          se2 <- s2.get(lbl).toRight(s"Error obtaining label $lbl in $s2")
        } yield (se1, se2)
        maybeSes match {
          case Left(err)         => { println(s"Error: $err"); false }
          case Right((se1, se2)) => b && compareShapeExprs(se1, se2)
        }
      })
    } else {
      println(s"Different labels: \n${s1.keys}\n${s2.keys}")
      false
    }
  }

  def compareShapeExprLists(se1: List[ShapeExpr], se2: List[ShapeExpr]): Boolean = {
    se1.zip(se2).foldLeft(true)((b, pair) => b && compareShapeExprs(pair._1, pair._2))
  }

  def compareShapeExprs(se1: ShapeExpr, se2: ShapeExpr): Boolean = {
    (se1, se2) match {
      case (ShapeOr(id1, ss1,_,_), ShapeOr(id2, ss2,_,_))   => id1 == id2 && compareShapeExprLists(ss1, ss2)
      case (ShapeAnd(id1, ss1,_,_), ShapeAnd(id2, ss2,_,_)) => id1 == id2 && compareShapeExprLists(ss1, ss2)
      case (ShapeNot(id1, s1,_,_), ShapeNot(id2, s2,_,_))   => id1 == id2 && compareShapeExprs(s1, s2)
      case (n1: NodeConstraint, n2: NodeConstraint) => Eq[NodeConstraint].eqv(n1,n2)
      case (s1: Shape, s2: Shape)                   => compareShapes(s1, s2)
      case (ShapeRef(i1,_,_), ShapeRef(i2,_,_))             => Eq[ShapeLabel].eqv(i1,i2)
      case (ShapeExternal(id1,_,_), ShapeExternal(id2,_,_)) => id1 == id2
      case (_, _)                                   => false
    }
  }

  def compareShapes(s1: Shape, s2: Shape): Boolean = {
    if (Eq[Shape].eqv(s1,s2)) true
    else {
      println(s"shapes are different: \n${s1}\n---\n${s2}")
      eqPrint(s1.isVirtual,s2.isVirtual) &&
        eqPrint(s1.isClosed,s2.isClosed) &&
        eqPrint(s1.expression, s2.expression) &&
        eqPrint(s1._extends, s2._extends) &&
        eqPrint(s1.actions, s2.actions)
    }
  }

  def eqPrint[A: Eq](x: A, y:A): Boolean = {
    if (Eq[A].eqv(x,y)) true
    else {
      println(s"Different:\n$x\n---\n$y")
      false
    }
  }

  def compareOptionBool(b1: Option[Boolean], b2: Option[Boolean]): Boolean = {
    b1 match {
      case None => b2 match {
        case None => true
        case Some(false) => true
        case Some(true) => false
      }
      case Some(false) => b2 match {
        case None => true
        case Some(false) => true
        case Some(true) => false
      }
      case Some(true) => b2 match {
        case Some(true) => true
        case _ => false
      }
    }
  }
}