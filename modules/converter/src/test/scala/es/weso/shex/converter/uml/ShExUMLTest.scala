package es.weso.shex.converter.uml

import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import es.weso.shex.converter.ShEx2UML
import es.weso.uml.UMLDiagram._
import org.scalatest.{FunSpec, Matchers}

class ShExUMLTest extends FunSpec with Matchers {

  describe("Dot") {
    it("Should generate plantuml diagram") {
      val field1 = UMLField("xsd:name", Some("http://schema.org/name"), List(ValueConstraint("xsd:string",Some("http;//xmlschema.org/string"))), Range(1,Unbounded))
      val field2 = UMLField("xsd:age", Some("http://schema.org/age"), List(ValueConstraint("xsd:int",Some("http;//xmlschema.org/int"))), Range(1,IntMax(1)))
      val field3 = UMLField("xsd:homePage", Some("http://schema.org/homePage"), List(ValueConstraint("IRI",None)), Range(1,IntMax(1)))
      val cls1 = UMLClass(1, ":User", Some("http://schema.org/User"), List(List(field1, field2)))
      val cls2 = UMLClass(2, ":Company", Some("http://schema.org/User"), List(List(field3)))
      val link1 = UMLLink(1,2,"schema:worksFor","http://schema.org", Star)
      val uml = UML(Map(IRILabel(IRI("L1")) -> 1, IRILabel(IRI("L2")) -> 2), Map(1 -> cls1, 2 -> cls2), List(link1))
      uml.classes.size should be(2)
      uml.links.length should be(1)
      // println(uml.toPlantUML)
    }
  }

  describe(s"ShEx2UML") {
    it(s"Should convert simple Shape with self-reference") {
    val shexStr =
      """|prefix : <http://example.org/>
         |
         |:User IRI {
         | :name IRI ;
         | :knows @:User ;
         | :worksFor @:Company
         |}
         |
         |:Company IRI {
         | :name IRI ;
         | :employee @:User *
         |}
      """.stripMargin
    val maybe = for {
      shex <- Schema.fromString(shexStr,"ShExC",None)
      uml <- ShEx2UML.schema2Uml(shex)
    } yield uml
    maybe.fold(
      e => fail(s"Error converting to UML: $e"),
      uml => {
        println(s"Converted uml:\n${uml.toPlantUML}")
        println(s"Converted to SVG:\n${uml.toSVG}")
      }
    )
  }

    it(s"Should convert value set with rdf:type") {
      val shexStr =
        """|prefix : <http://example.org/>
           |
           |:User {
           | :name [ :Person ] ;
           | :knows @:User ;
           |}
        """.stripMargin
      val maybe = for {
        shex <- Schema.fromString(shexStr,"ShExC",None)
        uml <- ShEx2UML.schema2Uml(shex)
      } yield uml
      maybe.fold(
        e => fail(s"Error converting to UML: $e"),
        uml => {
          println(s"Converted uml:\n${uml.toPlantUML}")
          println(s"Converted to SVG:\n${uml.toSVG}")
        }
      )
    }
  }

}