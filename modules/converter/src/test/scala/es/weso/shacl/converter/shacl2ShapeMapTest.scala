package es.weso.shacl.converter

import cats.implicits._
import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IOUtils
import org.scalatest.matchers.should._
import org.scalatest.funspec._
import es.weso.shapeMaps.Association
import es.weso.shapeMaps.ShapeMapLabel
import es.weso.shapeMaps.NodeSelector
import es.weso.shapeMaps.ShapeMap

class shacl2ShapeMapTest extends AnyFunSpec with Matchers {

  describe("shacl2ShapeMaps converter") {
    {
    shouldConvertSHACLShapeMap(
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |:S a sh:NodeShape ;
         |   sh:targetNode :x ;
         |   sh:nodeKind sh:IRI .
         """.stripMargin,
      """:x@:S""".stripMargin)

    shouldConvertSHACLShapeMap(
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         |:S a sh:NodeShape ;
         |   sh:targetClass :C ;
         |   sh:nodeKind sh:IRI .
         """.stripMargin,
      """{FOCUS rdf:type/rdfs:subClassOf* :C}@:S""".stripMargin)      

     shouldConvertSHACLShapeMap(
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         |:S a sh:NodeShape ;
         |   sh:targetSubjectsOf :p ;
         |   sh:nodeKind sh:IRI .
         """.stripMargin,
      """{FOCUS :p _}@:S""".stripMargin)            

           shouldConvertSHACLShapeMap(
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         |:S a sh:NodeShape ;
         |   sh:targetObjectsOf :p ;
         |   sh:nodeKind sh:IRI .
         """.stripMargin,
      """{_ :p FOCUS}@:S""".stripMargin)
    }
  }

  private def getAssociationPair(a: Association): (NodeSelector,ShapeMapLabel) = (a.node,a.shape)

  private def getPairs(shapeMap: ShapeMap): List[(NodeSelector,ShapeMapLabel)] = shapeMap.associations.map(getAssociationPair)

  def shouldConvertSHACLShapeMap(strSHACL: String, expected: String): Unit = {
    it(s"Should convert: $strSHACL to ShapeMap and obtain: $expected") {
    val cmp = RDFAsJenaModel.fromString(strSHACL, "TURTLE", None).flatMap(_.use(shaclRDF => for {
      shacl          <- RDF2Shacl.getShacl(shaclRDF)
      shapeMapConverted  <- IOUtils.fromES(Shacl2ShEx.shacl2ShEx(shacl).leftMap(e => s"Error in Shacl2ShEx conversion: $e"))
      expectedShapeMap <- IOUtils.fromES(shapeMaps.ShapeMap.fromString(expected, "Compact", None,shacl.pm,shacl.pm).leftMap(e => s"Error in Shape maps parsing: $e"))
    } yield (shapeMapConverted, expectedShapeMap, shacl)))
    cmp.attempt.unsafeRunSync().fold(
        e => fail(s"Error: $e"),
        values => {
          val (converted, expected, shacl) = values
          val (schema,shapeMap) = converted
          getPairs(shapeMap) should contain theSameElementsAs getPairs(expected)
        }
      )
    }
  }

}
