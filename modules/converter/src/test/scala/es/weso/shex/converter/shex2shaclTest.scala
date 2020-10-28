package es.weso.shex.converter

import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.Shacl2RDF
import org.scalatest.funspec.AnyFunSpec 
import org.scalatest.matchers.should._
import cats.implicits._
// import cats.data._ 
import cats.effect._
// import es.weso.utils.IOUtils._

class shex2shaclTest extends AnyFunSpec with Matchers {

  describe(s"ShEx2SHACL") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S IRI
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |   sh:nodeKind sh:IRI .
      """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S IRI AND BNode
      """.stripMargin,
      """prefix : <http://example.org/>
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
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:node [
        |      a sh:NodeShape ;
        |      sh:nodeKind sh:IRI ;
        |    ] ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
                       """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:string }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:node [
        |      a sh:NodeShape ;
        |      sh:datatype xsd:string ;
        |    ] ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
                       """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p [ "2019"^^xsd:integer ] }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:node [
        |      a sh:NodeShape ;
        |      sh:in ( "2019"^^xsd:integer )  ;
        |    ] ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
                       """.stripMargin)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p [1 "hi"@en "2018" ] }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:node [
        |      a sh:NodeShape ;
        |      sh:in (1 "hi"@en "2018" )  ;
        |    ] ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
                       """.stripMargin)

    /* For some reason the following test fails when checking isomorphism between 2 graphs
       It seems to be when using "2019"^^xsd:gYear (replacing it by xsd:integer works)
     */
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p [ "2019"^^xsd:gYear ] }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:qualifiedValueShape [
        |      a sh:NodeShape ;
        |      sh:in ( "2019"^^xsd:gYear )  ;
        |    ] ;
        |    sh:qualifiedMinCount 1 ;
        |    sh:qualifiedMaxCount 1
        |] .
        |""".stripMargin, true)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S {
         | :p . ;
         | :q .
         |}
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |    sh:node _:0
        |];
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |    sh:node _:0
        |] .
        |_:0 a sh:NodeShape .
        |""".stripMargin, false)

    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S EXTRA :p {
         | :p . ;
         | :q .
         |}
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#>
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:qualifiedMinCount 1 ;
        |    sh:qualifiedMaxCount 1 ;
        |    sh:qualifiedValueShape _:0
        |];
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |    sh:node _:0
        |] .
        |_:0 a sh:NodeShape .
        |""".stripMargin, false)

  }


  def shouldConvertShEx2Shacl(shexStr: String,
                              shaclStrExpected: String,
                              ignored: Boolean = false): Unit = {
    def comp(): Unit = {
      val result: IO[Boolean] = for {
        res1 <- RDFAsJenaModel.empty
        res2 <- RDFAsJenaModel.fromChars(shaclStrExpected,"TURTLE",None)
        vv <- (res1,res2).tupled.use{ case (rdfEmpty, rdfShaclExpected) => for {
        schema <- shex.Schema.fromString(shexStr,"SHEXC")
        shaclSchema <- ShEx2Shacl.shex2Shacl(schema,None).fold(
          ls => IO.raiseError(new RuntimeException(s"Errors converting ShEx2SHACL: ${ls.mkString("\n")}")),
          IO(_)
        )
        rdfShacl <- Shacl2RDF.shacl2RDF(shaclSchema, rdfEmpty)
        b <-rdfShacl.isIsomorphicWith(rdfShaclExpected)
        str1 <- rdfShacl.serialize("Turtle")
        str2 <- rdfShaclExpected.serialize("Turtle") 
        _ <- if (!b) IO(println(s"""|RDFs are not isomorphic. Obtained RDF:
             |${str1}
             |Expected:
             |${str2}
             |""".stripMargin))
          else IO(())
      } yield b}
      } yield vv

      result.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"),
        b => b should be(true))
    }
  val msg = (s"Should convert ShEx->SHACL\n$shexStr\nExpected:\n$shaclStrExpected\n--->")
  if (ignored) ignore(msg)(comp)
  else it(msg)(comp)
 }

}
