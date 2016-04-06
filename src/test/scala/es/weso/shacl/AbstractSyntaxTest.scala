package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._

object AbstractSyntaxTest extends FunSpec with Matchers {
  
describe("Abstract Syntax") {
  it("should be able to create a shape") {
    val c : Constraint =
      PropertyConstraint(
                id = None,
                predicate = IRI("http://example.org/p"),
                components = Seq(NodeKind(IRIType)))

    val shape = Shape(
        id = Some(IRI("http://example.org/s")),
        scopes =Seq(),
        filters= Seq(),
        component = Seq(c))
        
    println("Hi")        
    shape.id shouldBe defined 
    
  }
  
  }
}