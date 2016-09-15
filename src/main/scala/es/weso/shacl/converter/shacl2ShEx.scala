package es.weso.shacl.converter
import es.weso.shacl
import es.weso._
import es.weso.converter._
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._

object Shacl2ShEx extends Converter {
  
  
  def shacl2ShEx(schema:shacl.Schema): Result[shex.Schema] = {
    val rs: Seq[(shex.ShapeLabel,Result[shex.Shape])] = 
      schema.shapes.zipWithIndex.map{ case (s,i) => 
      (s.id.map(iri => mkIRILabel(iri)).getOrElse(mkBNodeLabel(i))
      , cnvShape(s)
      )
    }
    val r: Result[List[(shex.ShapeLabel,shex.Shape)]] = collect(rs.toList)
    val rMap: Result[Map[shex.ShapeLabel,shex.Shape]] = r.map(_.toMap)
    rMap.map(m => shex.Schema.empty.copy(shapes = Some(m)))
  }
  
  def collect[A,B,E](xs: List[(A,ValidatedNel[E,B])]): ValidatedNel[E,List[(A,B)]] = ???

  def cnvShape(s: shacl.Shape): Result[shex.Shape] = ???
  
  def mkIRILabel(iri: IRI): shex.ShapeLabel = 
    shex.IRILabel(iri)
  
  def mkBNodeLabel(n: Int): shex.ShapeLabel = 
    shex.BNodeLabel(BNodeId(n.toString))

}