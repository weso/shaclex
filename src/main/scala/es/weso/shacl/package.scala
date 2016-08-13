package es.weso
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import es.weso.rdf._
import es.weso.rdf.nodes._
import shacl.ViolationError._

package object shacl {

  implicit def showShape = new Show[Shape] {
    def show(shape: Shape): String = {
      shape.id.fold("_?")(iri => iri.str)
    }
  }

  implicit def showError = new Show[ViolationError] {
    def show(ve: ViolationError): String = {
      s"Violation Error(${ve.id}). Node(${ve.focusNode}) ${ve.message.getOrElse("")}"
    }
  }

  implicit def showEvidence = new Show[String] {
    def show(e: String): String = e
  }
  
  implicit def showRDFNode = new Show[RDFNode] {
    def show(n: RDFNode): String = n.toString
  }

  type ShapeTyping = Typing[RDFNode,Shape,ViolationError,String]
  
  type Comput = Fx.fx5[
    Reader[RDFReader,?], 
    State[ShapeTyping,?], 
    Choose, 
    Validate[ViolationError, ?], 
    Eval]

  type Result[A] =  Xor[NonEmptyList[ViolationError],List[(A,ShapeTyping)]]
  
  def isOK[A](r: Result[A]): Boolean = 
    r.isRight && r.toList.isEmpty == false  

  
 type Check[A] = Eff[Comput,A]
 
}