package es.weso.shacl.converter

import cats._
import cats.data._
import cats.implicits._
import es.weso.converter._
import es.weso.rdf.nodes._
import es.weso.{shacl, _}

object Shacl2ShEx extends Converter {
  
  def shacl2ShEx(schema:shacl.Schema): Result[shex.Schema] = {
    val rs: List[Result[shex.ShapeExpr]] = ??? // schema.shapes.map(cnvShape(_)).toList
    val r : Result[List[shex.ShapeExpr]] = rs.sequence
    r.map(m => shex.Schema.empty.copy(shapes = Some(m)))
  }
  
  def collect[A,B,E](xs: List[(A,ValidatedNel[E,B])]): 
      ValidatedNel[E,List[(A,B)]] = {
    val zero: ValidatedNel[E,List[(A,B)]] = Validated.valid(List())
    def comb(
        rest: ValidatedNel[E,List[(A,B)]],
        current: (A,ValidatedNel[E,B])): ValidatedNel[E,List[(A,B)]] = {
      val (a,r) = current
      (r |@| rest).map((b,ls) => (a,b) :: ls)
    }
    xs.foldLeft(zero)(comb)
  }

/*  def cnvShape(s: shacl.Shape): Result[shex.ShapeExpr] = {
    val id : Id = cnvId(s.id)
    val rs = s.propertyShapes.toList.map(cnvConstraint(id, _)).sequence
    rs.map(ses => ses.size match {
      case 1 => ses.head
      case n if n > 1 => shex.ShapeAnd(id,ses)
      case _ => ??? // TODO
    })
  } */

  def cnvId(id: Option[IRI]): Id = id.map(shex.IRILabel(_))
  
  def cnvShape(id: Id, c: shacl.Shape): Result[shex.ShapeExpr] =
     c match {
    case nc: shacl.NodeShape => cnvNodeShape(id,nc)
    case _ => err(s"cnvConstraint: Unimplemented $c")
  }

  type Id = Option[shex.ShapeLabel]

  def cnvNodeShape(id: Id, c: shacl.NodeShape): Result[shex.ShapeExpr] = {
    val rs = c.components.toList.map(cnvComponent(id,_)).sequence
    rs.map(ses => ses.size match {
      case 1 => ses.head
      case n if n > 1 => shex.ShapeAnd(id,ses)
      case _ => ???
    })
  }
  
  def cnvComponent(id: Id, c: shacl.Component): Result[shex.NodeConstraint] = {
    c match {
      case shacl.NodeKind(shacl.IRIKind) => ok(shex.NodeConstraint.nodeKind(id, shex.IRIKind, List()))
      case shacl.NodeKind(shacl.BlankNodeKind) => ok(shex.NodeConstraint.nodeKind(id, shex.BNodeKind, List()))
      case _ => err(s"cnvComponent: Unimplemented $c")
    }
  }
  
  def mkIRILabel(iri: IRI): shex.ShapeLabel = 
    shex.IRILabel(iri)
  
  def mkBNodeLabel(n: Int): shex.ShapeLabel = 
    shex.BNodeLabel(BNodeId(n.toString))

}