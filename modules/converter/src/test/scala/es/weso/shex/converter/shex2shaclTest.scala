package es.weso.shex.converter

import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.Shacl2RDF
// import org.scalatest.funspec.AnyFunSpec 
// import org.scalatest.matchers.should._
import cats.implicits._
import cats.effect._
import munit._

class shex2shaclTest extends CatsEffectSuite {

  test("simple IRI") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S IRI
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        |prefix xsd:   <http://www.w3.org/2001/XMLSchema#> 
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
        |
        |:S a sh:NodeShape ;
        |   sh:nodeKind sh:IRI .
      """.stripMargin)
  }

  test("p dot") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S { :p . }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        |prefix xsd:   <http://www.w3.org/2001/XMLSchema#> 
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
        |
        |:S a sh:NodeShape ;
        |   sh:property [ 
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
      """.stripMargin)
  }


  test("IRI AND BNode") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S IRI AND BNode
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        |prefix xsd:   <http://www.w3.org/2001/XMLSchema#> 
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
        |
        |:S a sh:NodeShape ;
        |   sh:and (
        |    [ a sh:NodeShape; sh:nodeKind sh:IRI ]
        |    [ a sh:NodeShape; sh:nodeKind sh:BlankNode ]
        |   ) .""".stripMargin)
  }

  test("IRI") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |:S { :p IRI }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        |prefix xsd:   <http://www.w3.org/2001/XMLSchema#> 
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:nodeKind sh:IRI ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .""".stripMargin)
  }

  test("p xsd:string") {
    shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:string }
      """.stripMargin,
      """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        |prefix xsd:   <http://www.w3.org/2001/XMLSchema#> 
        |prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
        |
        |:S a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :p ;
        |    sh:datatype xsd:string ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
        |""".stripMargin)
  }

  test("p valueSet") {
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
        |    sh:in ( "2019"^^xsd:integer )  ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
        |""".stripMargin)
    }

    test("valueSet 1 lang string") {
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
        |    sh:in (1 "hi"@en "2018" )  ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
                       """.stripMargin)
    }

    /* For some reason the following test fails when checking isomorphism between 2 graphs
       It seems to be when using "2019"^^xsd:gYear (replacing it by xsd:integer works)
     */
    test("p valueSet gYear") {
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
        |    sh:in ( "2019"^^xsd:gYear )  ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1
        |] .
        |""".stripMargin)
    }

    test("eachOf") {
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
        |];
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |] .
        |""".stripMargin)
    }

    test("shapeRef") {
      shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S {
         | :p @:T ;
         |}
         |:T {
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
        |    sh:node :T
        |] .
        |
        |:T a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |] .
        |""".stripMargin)
    }

    test("OR") {
      shouldConvertShEx2Shacl(
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S {
         | :p IRI OR @:T OR @:S +;
         |}
         |:T {
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
        |    sh:or ( [ a sh:NodeShape ;
        |              sh:nodeKind  sh:IRI 
        |            ]
        |            [ a        sh:NodeShape ;
        |              sh:node  :T
        |            ]
        |            [ a        sh:NodeShape ;
        |              sh:node  :S
        |            ]
        |          )
        |] .
        |
        |:T a sh:NodeShape ;
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |] .
        |""".stripMargin)
    }

    test("eachOf EXTRA") {
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
        |    sh:qualifiedValueShape [] ;
        |];
        |   sh:property [
        |    a sh:PropertyShape ;
        |    sh:path :q ;
        |    sh:minCount 1 ;
        |    sh:maxCount 1 ;
        |] .
        |""".stripMargin)
  } 


  def shouldConvertShEx2Shacl(shexStr: String,
                              shaclStrExpected: String,
                             ): Unit = {
      val result: IO[(String,String)] = for {
        res1 <- RDFAsJenaModel.empty
        res2 <- RDFAsJenaModel.fromChars(shaclStrExpected,"TURTLE",None)
        vv <- (res1,res2).tupled.use{ case 
          (rdfEmpty, rdfShaclExpected) => for {
            schema <- shex.Schema.fromString(shexStr,"SHEXC")
            shaclSchema <- ShEx2Shacl.shex2Shacl(schema,None) match {
              case Left(s) => {
                println(s"Error converting: ${s.mkString("\n")}")
                IO.raiseError(new RuntimeException(s.mkString("\n")))
              }
              case Right(s) => IO(s)
            }
            rdfShacl <- Shacl2RDF.shacl2RDF(shaclSchema, rdfEmpty)
            b <-rdfShacl.isIsomorphicWith(rdfShaclExpected)
            str1 <- rdfShacl.serialize("Turtle")
            str2 <- rdfShaclExpected.serialize("Turtle")
            _ <- if (str1 != str2) IO (println(s"Obtained:\n${str1}\nExpected:\n${str2}")) 
                 else IO(())
          } yield (str1,str2)}
      } yield vv

    result.map(pair => assertEquals(pair._1,pair._2))
  /*val msg = (s"${name}: should convert ShEx->SHACL\n$shexStr\nExpected:\n$shaclStrExpected\n--->")
  if (ignored) ignore(msg)(comp)
  else it(msg)(comp) */
 }

}
