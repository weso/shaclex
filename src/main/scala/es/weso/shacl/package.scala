package es.weso
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import es.weso.rdf._
import es.weso.rdf.nodes._
import shacl.ViolationError._

package object shacl {
  
  type Comput = Fx.fx5[
    Reader[RDFReader,?], 
    State[Typing,?], 
    Choose, 
    Validate[ViolationError, ?], 
    Eval]

  type Result[A] =  Xor[NonEmptyList[ViolationError],List[(A,Typing)]]
  
  def isOK[A](r: Result[A]): Boolean = 
    r.isRight && r.toList.isEmpty == false  

  
 type Check[A] = Eff[Comput,A]
  
}