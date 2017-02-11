package es.weso.shacl.converter
import es.weso.shacl
import es.weso._
import es.weso.converter._
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._

object Shacl2ShEx extends Converter {
  
  def shacl2ShEx(schema:shacl.Schema): Result[shex.Schema] = {
    val rs: Seq[(shex.ShapeLabel,Result[shex.ShapeExpr])] = 
      schema.shapes.zipWithIndex.map{ case (s,i) => 
      (s.id.map(iri => mkIRILabel(iri)).getOrElse(mkBNodeLabel(i))
      , cnvShape(s)
      )
    }
    val r: Result[List[(shex.ShapeLabel,shex.ShapeExpr)]] = collect(rs.toList)
    val rMap: Result[Map[shex.ShapeLabel,shex.ShapeExpr]] = r.map(_.toMap)
    rMap.map(m => shex.Schema.empty.copy(shapes = Some(m)))
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

  def cnvShape(s: shacl.NodeShape): Result[shex.ShapeExpr] = {
    val rs = s.constraints.toList.map(cnvConstraint(_)).sequence
    rs.map(ses => ses.size match {
      case 1 => ses.head
      case n if n > 1 => shex.ShapeAnd(ses)
      case _ => ??? // TODO
    })
  }
  
  def cnvConstraint(c: shacl.Shape): Result[shex.ShapeExpr] =
     c match {
    case nc: shacl.NodeConstraint => cnvNodeConstraint(nc)
    case _ => err(s"cnvConstraint: Unimplemented $c")
  }
  
  def cnvNodeConstraint(c: shacl.NodeConstraint): Result[shex.ShapeExpr] = { 
    val rs = c.components.toList.map(cnvComponent(_)).sequence
    rs.map(ses => ses.size match {
      case 1 => ses.head
      case n if n > 1 => shex.ShapeAnd(ses)
      case _ => ???
    })
  }
  
  def cnvComponent(c: shacl.Component): Result[shex.NodeConstraint] = {
    c match {
      case shacl.NodeKind(shacl.IRIKind) => ok(shex.NodeConstraint.nodeKind(shex.IRIKind, List()))
      case shacl.NodeKind(shacl.BlankNodeKind) => ok(shex.NodeConstraint.nodeKind(shex.BNodeKind, List()))
      case _ => err(s"cnvComponent: Unimplemented $c")
    }
  }
  
  def mkIRILabel(iri: IRI): shex.ShapeLabel = 
    shex.IRILabel(iri)
  
  def mkBNodeLabel(n: Int): shex.ShapeLabel = 
    shex.BNodeLabel(BNodeId(n.toString))

}