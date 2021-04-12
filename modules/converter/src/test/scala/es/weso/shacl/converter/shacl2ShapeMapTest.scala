package es.weso.shacl.converter

import cats.implicits._
import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IOUtils
import munit._
import es.weso.shapemaps.Association
import es.weso.shapemaps.ShapeMapLabel
import es.weso.shapemaps.NodeSelector
import es.weso.shapemaps.ShapeMap

class shacl2ShapeMapTest extends CatsEffectSuite {

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

  private def getAssociationPair(a: Association): (NodeSelector,ShapeMapLabel) = (a.node,a.shape)

  private def getPairs(shapeMap: ShapeMap): List[(NodeSelector,ShapeMapLabel)] = shapeMap.associations.map(getAssociationPair)

  def shouldConvertSHACLShapeMap(strSHACL: String, expected: String): Unit = {
    test(s"Should convert: $strSHACL to ShapeMap and obtain: $expected") {

    val cmp = RDFAsJenaModel.fromString(strSHACL, "TURTLE", None).flatMap(_.use(shaclRDF => for {
      shacl          <- RDF2Shacl.getShacl(shaclRDF)
      shapeMapConverted  <- IOUtils.fromES(Shacl2ShEx.shacl2ShEx(shacl).leftMap(e => s"Error in Shacl2ShEx conversion: $e"))
      expectedShapeMap <- IOUtils.fromES(shapemaps.ShapeMap.fromString(expected, "Compact", None,shacl.pm,shacl.pm).leftMap(e => s"Error in Shape maps parsing: $e"))
     } yield (shapeMapConverted, expectedShapeMap, shacl)))
    cmp.map(values => {
          val (converted, expected, shacl) = values
          val (schema,shapeMap) = converted
          assertEquals(getPairs(shapeMap), getPairs(expected))
        }
      )
    }
  }

}
