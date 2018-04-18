package es.weso.shex.converter.uml

import es.weso.uml.UMLDiagram._
import org.scalatest.{FunSpec, Matchers}

class ShExUMLTest extends FunSpec with Matchers {

  describe("Dot") {
    it("Should generate plantuml diagram") {
      val field1 = UMLField("xsd:name", "http://schema.org/name", List(ValueConstraint("xsd:string",Some("http;//xmlschema.org/string"))), Range(1,Unbounded))
      val field2 = UMLField("xsd:age", "http://schema.org/age", List(ValueConstraint("xsd:int",Some("http;//xmlschema.org/int"))), Range(1,IntMax(1)))
      val field3 = UMLField("xsd:homePage", "http://schema.org/homePage", List(ValueConstraint("IRI",None)), Range(1,IntMax(1)))
      val cls1 = UMLClass("C1", ":User", "http://schema.org/User", List(field1, field2))
      val cls2 = UMLClass("C2", ":Company", "http://schema.org/User", List(field3))
      val link1 = UMLLink(cls1,cls2,"schema:worksFor","http://schema.org", Star)
      val uml = UML(Map("C1" -> cls1, "C2" -> cls2), List(link1))
      println(uml.toPlantUML)
    }
  }
}