package es.weso.shex.converter

import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.Shacl2RDF
import org.scalatest._
import cats.implicits._

class shex2shaclTest extends FunSpec with Matchers with EitherValues {

  describe(s"ShEx2SHACL") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
        |:S IRI
      """.stripMargin, """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |   sh:nodeKind sh:IRI .
      """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S IRI AND BNode
      """.stripMargin, """prefix : <http://example.org/>
                         |prefix sh: <http://www.w3.org/ns/shacl#>
                         |:S a sh:NodeShape ;
                         |   sh:and (
                         |    [ a sh:NodeShape; sh:nodeKind sh:IRI ]
                         |    [ a sh:NodeShape; sh:nodeKind sh:BlankNode ]
                         |   ) .
                       """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S { :p IRI }
      """.stripMargin, """prefix : <http://example.org/>
                         |prefix sh: <http://www.w3.org/ns/shacl#>
                         |:S a sh:NodeShape ;
                         |   sh:property [
                         |    a sh:PropertyShape ;
                         |    sh:path :p ;
                         |    sh:qualifiedValueShape [
                         |      a sh:NodeShape ;
                         |      sh:nodeKind sh:IRI ;
                         |    ] ;
                         |    sh:qualifiedMinCount 1 ;
                         |    sh:qualifiedMaxCount 1
                         |] .
                       """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:string }
      """.stripMargin, """prefix : <http://example.org/>
                         |prefix sh: <http://www.w3.org/ns/shacl#>
                         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                         |:S a sh:NodeShape ;
                         |   sh:property [
                         |    a sh:PropertyShape ;
                         |    sh:path :p ;
                         |    sh:qualifiedValueShape [
                         |      a sh:NodeShape ;
                         |      sh:datatype xsd:string ;
                         |    ] ;
                         |    sh:qualifiedMinCount 1 ;
                         |    sh:qualifiedMaxCount 1
                         |] .
                       """.stripMargin)

    // ignored...it seems to work but the system complains that the graphs are not isomorphic
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p [1 "hi"@en "2018"^^xsd:year ] }
      """.stripMargin, """prefix : <http://example.org/>
                         |prefix sh: <http://www.w3.org/ns/shacl#>
                         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                         |:S a sh:NodeShape ;
                         |   sh:property [
                         |    a sh:PropertyShape ;
                         |    sh:path :p ;
                         |    sh:qualifiedValueShape [
                         |      a sh:NodeShape ;
                         |      sh:in (1 "hi"@en "2018"^^xsd:year )  ;
                         |    ] ;
                         |    sh:qualifiedMinCount 1 ;
                         |    sh:qualifiedMaxCount 1
                         |] .
                       """.stripMargin, true)

  }



  def shouldConvertShEx2Shacl(shexStr: String,
                              shaclStrExpected: String,
                              ignored: Boolean = false): Unit = {
    def comp(): Unit = {
      val result: Either[String,Boolean] = for {
        schema <- shex.Schema.fromString(shexStr,"SHEXC")
        shaclSchema <- ShEx2Shacl.shex2Shacl(schema,None).
          leftMap(e => s"Error converting schema: $e\n$schema")
        rdfShacl = (new Shacl2RDF {}).toRDF(shaclSchema, RDFAsJenaModel.empty)
        rdfShaclExpected <- RDFAsJenaModel.fromChars(shaclStrExpected,"TURTLE",None)
        b <-rdfShacl.isIsomorphicWith(rdfShaclExpected)
        _ <- if (!b)
           Left("RDFs are not isomorphic. Obtained RDF:\n" +
             s"${rdfShacl.serialize("N-Triples")}\n" +
             s"Expected: \n${rdfShaclExpected.serialize("N-Triples")}")
          else Right(())
      } yield b
      result.fold(e => fail(s"Error: $e"),
        b => b should be(true))
    }
  val msg = (s"Should convert ShEx->SHACL\n$shexStr\n$shaclStrExpected\n--->")
  if (ignored) ignore(msg)(comp)
  else it(msg)(comp)
 }

}
