package es.weso.schema

import org.scalatest._
import matchers.should._
import funspec._
import cats._
import cats.implicits._
import cats.effect._
import es.weso.utils.json.JsonCompare.jsonDiff
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IOUtils._
import es.weso.utils._
import io.circe.parser._

class SchemaConversionsTest extends AnyFunSpec with Matchers with EitherValues {

  type PureCompare = (String,String) => Either[String,Boolean]
  type IOCompare = (String,String) => IO[Boolean]
  def cnvCompare[A](cmp: PureCompare): IOCompare = (x,y) => fromES(cmp(x,y))

  describe(s"Convert 2 shaclex") {
      val str1 =
        """|prefix sh: <http://www.w3.org/ns/shacl#>
           |<x> a sh:NodeShape .
           |""".stripMargin
      val strExpected =
        """
          |<x> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/shacl#NodeShape> .
        """.stripMargin
      shouldConvert(str1, "Turtle", "Shaclex", "N-Triples", "Shaclex", strExpected, rdfCompare)
      val str2 =
        """|prefix sh: <http://www.w3.org/ns/shacl#>
           |<y> a sh:NodeShape .
           |""".stripMargin
      val str2Expected =
        """
          |<y> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/shacl#NodeShape> .
        """.stripMargin
      shouldConvert(str2, "Turtle", "Shaclex", "N-Triples", "Shaclex", str2Expected, rdfCompare)
  }

  describe("ShExC -> ShExJ") {
    val strShExC =
      """
          |prefix : <http://example.org/>
          |:S { :p IRI }
        """.stripMargin

    val strExpected =
      """
          | {
          |   "type" : "Schema",
          |   "@context" : "http://www.w3.org/ns/shex.jsonld",
          |   "shapes" : [
          |     {
          |       "type" : "Shape",
          |       "id" : "http://example.org/S",
          |       "expression" : {
          |         "type" : "TripleConstraint",
          |         "predicate" : "http://example.org/p",
          |         "valueExpr" : {
          |           "type" : "NodeConstraint",
          |           "nodeKind" : "iri"
          |         }
          |       }
          |     }
          |   ]
          |}
        """.stripMargin
    shouldConvert(strShExC, "ShExC", "ShEx", "ShExJ", "ShEx", strExpected, cnvCompare(jsonCompare))
  }

  describe(s"ShExC -> Turtle")  {
      val strShExC =
        """
          |prefix : <http://example.org/>
          |:S { :p IRI }
        """.stripMargin

      val strExpected =
        """
          |@prefix :      <http://example.org/> .
          |@prefix sx:      <http://shex.io/ns/shex#> .
          |
          |:S a    sx:Shape ;
          |   sx:closed false ;
          |   sx:expression [
          |    a sx:TripleConstraint ;
          |    sx:predicate :p ;
          |    sx:valueExpr [
          |     a       sx:NodeConstraint ;
          |    sx:nodeKind sx:iri ]
          | ] .
          |
          |[ a  sx:Schema ;
          |  sx:shapes :S
          |] .
        """.stripMargin
      shouldConvert(strShExC, "ShExC", "ShEx", "Turtle", "ShEx", strExpected, rdfCompare)
  }
  describe(s"SHACL (Turtle) -> SHACL (JSON-LD)")  {
    val strShacl =
      """
        |prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |	sh:nodeKind sh:IRI
      """.stripMargin

    val strExpected =
      """
        |<http://example.org/S> <http://www.w3.org/ns/shacl#nodeKind> <http://www.w3.org/ns/shacl#IRI> .
        |<http://example.org/S> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/shacl#NodeShape> .
      """.stripMargin
    shouldConvert(strShacl, "Turtle", "Shaclex", "N-Triples", "Shaclex", strExpected, rdfCompare)
  }

  describe(s"SHACL (Turtle) -> ShEx (ShExJ)")  {
    val strShacl =
      """
        |prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |	sh:nodeKind sh:IRI
      """.stripMargin

    val strExpected = """
      | { "type" : "Schema",
      |   "@context" : "http://www.w3.org/ns/shex.jsonld",
      |   "shapes" : [ {
      |      "type" : "NodeConstraint",
      |         "id" : "http://example.org/S",
      |         "nodeKind" : "iri"
      |  } ] }
    """.stripMargin
    shouldConvert(strShacl, "Turtle", "Shaclex", "ShExJ", "ShEx", strExpected, cnvCompare(jsonCompare))
  }

  describe(s"SHACL (Turtle) -> ShEx (ShExC)")  {
    val strShacl =
      """
        |prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |	sh:nodeKind sh:IRI
      """.stripMargin

    val strExpected = """
       |<http://example.org/S> IRI
       |""".stripMargin
    shouldConvert(strShacl, "Turtle", "Shaclex", "ShExC", "ShEx", strExpected, shExCompare)
  }

  describe(s"ShEx (ShExC) -> SHACL (Turtle)")  {
    val strShacl =
      """
        |prefix : <http://example.org/>
        |:S IRI
      """.stripMargin

    val strExpected = """
                        |prefix : <http://example.org/>
                        |prefix sh: <http://www.w3.org/ns/shacl#>
                        |:S a sh:NodeShape ;
                        |	sh:nodeKind sh:IRI
                        |""".stripMargin
    shouldConvert(strShacl, "ShExC", "ShEx", "Turtle", "SHACLEX", strExpected, rdfCompare)
  }


  def shouldConvert(str: String, format: String, engine: String,
                    targetFormat: String, targetEngine: String,
                    expected: String,
                    compare: (String,String) => IO[Boolean]
                   ): Unit = {
   it(s"Should convert $str with format $format and engine $engine and obtain $expected") {
     val r: IO[(Schema,String,Boolean)] = for {
      schema  <- runWithError(Schemas.fromString(str, format, engine, None),s"Error reading Schema ($format/$engine): $str")
      // _ <- { info(s"str:\n$str, format: $format, engine: $engine\nSchema: $schema)"); Right(())}
      strConverted <- runWithError(schema.convert(Some(targetFormat), Some(targetEngine),None), s"Error converting schema(${schema.name}) to ($targetFormat/$targetEngine")
      // _ <- { info(s"strConverted:\n$strConverted\ntargetFormat: $targetFormat, targetEngine: $targetEngine"); Right(())}
      result       <- runWithError(compare(strConverted, expected), s"Error in comparison")
    } yield // (strConverted, expected, result)
       (schema, strConverted, result)

    r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), v => {
       val (s1, s2, r) = v
       if (r) {
         info(s"Conversion is ok")
       } else {
         fail(s"Different results\ns1=$s1\ns2$s2")
       }
     })
   }
  }

  def jsonCompare(s1: String, s2: String): Either[String, Boolean] = for {
    json1 <- parse(s1).leftMap(e => s"Error parsing $s1\n$e")
    json2 <- parse(s2).leftMap(e => s"Error parsing $s2\n$e")
    b <-
      if (json1.equals(json2)) { Right(true)}
      else Left(s"Json's different:\nJson1: $json1\nJson2: $json2. Diff: ${jsonDiff(json1, json2)}")
  } yield b

  def rdfCompare(s1: String, s2: String): IO[Boolean] = (
    RDFAsJenaModel.fromChars(s1,"TURTLE",None),
    RDFAsJenaModel.fromChars(s2,"TURTLE",None)
  ).tupled.use { 
    case (rdf1,rdf2) => for {
     b <- rdf1.isIsomorphicWith(rdf2)
  } yield b }

  def shExCompare(s1: String, s2: String): IO[Boolean] = for {
    schema1 <- runWithError(Schemas.fromString(s1,"ShExC","ShEx",None), s"Error reading ShEx from string s1: $s1")
    // _ <- { info(s"Schema1: $schema1"); Right(()) }
    schema2 <- runWithError(Schemas.fromString(s2,"ShExC","ShEx",None), s"Error reading ShEx from string s1: $s1")
    // _ <- { info(s"Schema2: $schema2"); Right(()) }
    json1 <- runWithError(schema1.convert(Some("ShExJ"),Some("ShEx"),None), s"Error converting schema1 to ShEx/ShExJ")
    // _ <- { info(s"Json1: $json1"); Right(()) }
    json2 <- runWithError(schema2.convert(Some("ShExJ"),Some("ShEx"),None), s"Error converting schema2 to ShEx/ShExJ")
    // _ <- { info(s"Json2: $json2"); Right(()) }
    b <- IOUtils.fromES(jsonCompare(json1,json2))
  } yield b

  // TODO: Move to IOUtils
  def runWithError[A](x: IO[A], msg: String): IO[A] = {
    MonadError[IO,Throwable].adaptError(x) {
      case e : Exception => IOException.fromString(s"$msg: ${e.getMessage}")
    }
  }

}
