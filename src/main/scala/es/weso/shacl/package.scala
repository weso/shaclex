package es.weso
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import es.weso.rdf._
import es.weso.rdf.nodes._
import shacl.ViolationError._
import ErrorEffect.ErrorOrOk


package object shacl {

 implicit def showShape = new Show[Shape] {
    def show(shape: Shape): String = {
      shape.id.fold("_?")(iri => iri.str)
    }
  }

 implicit def showRDFNode = new Show[RDFNode] {
    def show(n: RDFNode): String = n.toString
  }

 type ShapeTyping = Typing[RDFNode,Shape,String,String]
 
 type Comput = Fx.fx4[
    Reader[RDFReader,?], 
    Reader[ShapeTyping,?],
    Writer[Evidence,?],
    ErrorOrOk]

 type Check[A] = Eff[Comput,A]
 
}