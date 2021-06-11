package es.weso.schemaInfer
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.rdf.parser.RDFParser
import es.weso.schema.{Schemas, ShExSchema}
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._
// import cats.data._
import cats.implicits._
import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec._

class SchemaInterTest extends AnyFunSpec with Matchers with RDFParser {

  
  describe(s"Schema Infer") {


    checkSchemaInfer(
      """|prefix schema: <http://schema.org/>
         |prefix wdp: <http://www.wikidata.org/prop/>
         |prefix prov: <http://www.w3.org/ns/prov#>
         |prefix wd: <http://www.wikidata.org/entity/>
         |prefix ps: <http://www.wikidata.org/entity/statement/>
         |prefix wdref: <http://www.wikidata.org/reference/>
         |prefix pr: <http://www.wikidata.org/prop/reference/>
         |prefix prv: <http://www.wikidata.org/prop/reference/value/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |wd:Q31270287 wdp:P1476 ps:Q31270287-AFE7D074-CF36-4950-B131-CDBEE14CB3D1 .
         |
         |ps:Q31270287-AFE7D074-CF36-4950-B131-CDBEE14CB3D1 prov:wasDerivedFrom wdref:62da53368e17402c77b72fa7c9388a70d69ee1df .
         |
         |wdref:62da53368e17402c77b72fa7c9388a70d69ee1df pr:P248 <http://www.wikidata.org/entity/Q5412157> .
         |wdref:62da53368e17402c77b72fa7c9388a70d69ee1df prv:P813 <http://www.wikidata.org/value/1ffdc99e89212d982d6a69ba46106fe1> .
         |wdref:62da53368e17402c77b72fa7c9388a70d69ee1df pr:P698 <http://www.wikidata.org/value/1ffdc99e89212d982d6a69ba46106fe1> .
         |wdref:62da53368e17402c77b72fa7c9388a70d69ee1df pr:P813 "2017-07-01"^^xsd:date .
         |
         |""".stripMargin,
/*      """|prefix schema: <http://schema.org/>
         |prefix wdp: <http://www.wikidata.org/prop/>
         |prefix prov: <http://www.w3.org/ns/prov#>
         |prefix wd: <http://www.wikidata.org/entity/>
         |prefix ps: <http://www.wikidata.org/entity/statement/>
         |prefix wdref: <http://www.wikidata.org/reference/>
         |prefix pr: <http://www.wikidata.org/prop/reference/>
         |prefix prv: <http://www.wikidata.org/prop/reference/value/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | wdp:P1476 @<P1476Prop> ;
         |}
         |
         |<P1476Prop> {
         | prov:wasDerivedFrom @<P1476PropRef>
         |}
         |
         |<P1476PropRef> {
         | pr:P248 IRI ;
         | prv:P813 IRI ;
         | pr:P698  IRI ;
         | pr:P813 xsd:date
         |} 
      """.stripMargin */
      """|prefix pr: <http://www.wikidata.org/prop/reference/>
         |prefix ps: <http://www.wikidata.org/entity/statement/>
         |prefix prv: <http://www.wikidata.org/prop/reference/value/>
         |prefix wdref: <http://www.wikidata.org/reference/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |prefix schema: <http://schema.org/>
         |prefix wdp: <http://www.wikidata.org/prop/>
         |prefix wd: <http://www.wikidata.org/entity/>
         |prefix prov: <http://www.w3.org/ns/prov#>
         |prefix sx: <http://weso.es/ns/shex/>
         |<S> {
         | wdp:P1476  IRI 
         |}
         |""".stripMargin
      , "wd:Q31270287", "S"
    )

    checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name      xsd:string ;
         |}
      """.stripMargin, ":alice", "S"
    )

  /*  checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" ;
                        |       schema:knows :bob .
                        |:bob   schema:name "Bob" .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name      xsd:string ;
         | schema:knows     @<knowsShape>
         |}
         |<knowsShape> {
         | schema:name      xsd:string
         |}
      """.stripMargin, ":alice", "S"
    ) */

    checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" ;
                        |       schema:knows :alice .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name      xsd:string ;
         | schema:knows     IRI # Maybe infer @<S>
         |}
      """.stripMargin, ":alice", "S"
    )

  /*  checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" ;
                        |       schema:knows :bob ;
                        |       schema:worksFor :company .
                        |
                        |:bob  schema:name "Robert" .
                        |
                        |:company schema:name "Company" .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name      xsd:string ;
         | schema:knows     @<knowsShape> ;
         | schema:worksFor  @<worksForShape>
         |}
         |<worksForShape> {
         | schema:name     xsd:string
         |}
         |<knowsShape> {
         | schema:name xsd:string
         |}
      """.stripMargin, ":alice", "S"
    ) */


    checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix p: <http://www.wikidata.org/prop>
                        |prefix pr: <http://www.wikidata.org/prop/reference>
                        |prefix prov: <http://www.w3.org/ns/prov#>
                        |:x :p 1, "Hi" ;
                        |   :q "Hi" ;
                        |   :r :xs .
                        |:xs prov:wasDerivedFrom :r .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |<S> {
         | :p Literal     {1,*};
         |  :q xsd:string ;
         |  :r IRI
         |}
      """.stripMargin, ":x", "S"
    )


    checkSchemaInfer("""|prefix : <http://example.org/>
                        |:x :p 1, "Hi" ;
                        |   :q "Hi" ;
                        |   :r :x .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |<S> {
         | :p Literal     {1,*};
         |  :q xsd:string ;
         |  :r IRI
         |}
      """.stripMargin, ":x", "S"
    )

    checkSchemaInfer("""|prefix : <http://example.org/>
                        |:x :p 1, 2, 3 ;
                        |   :q "Hi"@en ;
                        |   :r _:1 .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |<S> {
         | :p xsd:integer *;
         | :q [ @en ] ;
         | :r BNode
         |}
      """.stripMargin, ":x", "S"
    )

  /*  checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:knows :bob .
                        |:bob  schema:knows :carol .
                        |:carol schema:knows :dave .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:knows @<knowsShape> ;
         |}
         |<knowsShape> {
         | schema:knows @<knowsShape2>
         |}
         |<knowsShape2> {
         | schema:knows IRI
         |}
         """.stripMargin, ":alice", "S"
    ) */

  /*  checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" ;
                        |       schema:knows :bob .
                        |:bob  schema:knows :carol ;
                        |      schema:name "Robert" .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name xsd:string ;
         | schema:knows @<knowsShape> ;
         |}
         |<knowsShape> {
         | schema:name xsd:string ;
         | schema:knows IRI
         |}
      """.stripMargin, ":alice", "S"
    ) */
 
  /*  checkSchemaInfer("""|prefix : <http://example.org/>
                        |prefix schema: <http://schema.org/>
                        |
                        |:alice schema:name "Alice" ;
                        |       schema:knows :bob, :alice .
                        |:bob  schema:knows :carol ;
                        |      schema:name "Robert" .
                     """.stripMargin,
      """|prefix :    <http://example.org/>
         |prefix schema: <http://schema.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | schema:name xsd:string ;
         | schema:knows @<knowsShape>* ;
         |}
         |<knowsShape> {
         | schema:name xsd:string ;
         | schema:knows IRI
         |}
      """.stripMargin, ":alice", "S"
    ) */

  /*  checkSchemaInfer("""|@prefix :      <http://example.org/thing> .
                        |@prefix td:    <http://www.w3.org/ns/td#> .
                        |@prefix js:    <http://www.w3.org/ns/json-schema#> .
                        |
                        |:light  td:actions      [ td:transition  [ td:description  "smooth transition from current brightness level to target brightness level" ;
                        |                                           td:input        []
                        |                                         ] ] ;
                        |        td:description  "a dimmable light with smooth transitions" ;
                        |        td:name         "light" ;
                        |        td:properties   [ td:brightness  [ js:maximum      100 ;
                        |                                           js:minimum      0 ;
                        |                                           js:type         "number" ;
                        |                                           td:description  "brightness in range 0 to 100"
                        |                                         ] ] .
                     """.stripMargin,
      """|prefix :      <http://example.org/thing>
         |prefix td:    <http://www.w3.org/ns/td#>
         |prefix js:    <http://www.w3.org/ns/json-schema#>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |<S> {
         | td:actions @<actionsShape> ;
         | td:desctiption xsd:string ;
         | td:name xsd:string ;
         | td:properties @<propertiesShape>
         |}
         |<actionsShape> {
         | td:transition @<transitionShape> ;
         |}
         |<propertiesshape> {
         | td:properties @<brightnessShape>
         |}
         |<brightnessShape> {
         | js:minimum xsd:integer ;
         | js:maximum xsd:integer ;
         | js:type xsd:string ;
         | td:description xsd:string
         |}
      """.stripMargin, ":light", "S"
    ) */

  // TODO: Move the following code to some common tools library
  case class TestException(msg: String) extends RuntimeException(msg)
  def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(IO.raiseError, IO.pure)
  def fromEitherS[A](e: Either[String, A]): IO[A] = fromEither(e.leftMap(TestException))


  def checkSchemaInfer(rdfStr: String, expectedStr: String, nodeSelectorStr: String, label: String): Unit = {
    it(s"Should infer a ShEx schema:\n$rdfStr\n for node $nodeSelectorStr and obtain\n$expectedStr\n") {
      val result = RDFAsJenaModel.fromString(rdfStr, "TURTLE").flatMap(_.use(rdf => for {
       schemaExpected <- Schemas.fromString(expectedStr,"ShExC","ShEx")
       pm <- rdf.getPrefixMap
       nodeSelector <- fromEitherS(NodeSelector.fromString(nodeSelectorStr, None,pm))
       eitherPair <- SchemaInfer.runInferSchema(rdf, nodeSelector, "ShEx", IRI("S"))
       pair <- fromEitherS(eitherPair)
      } yield (rdf, schemaExpected, nodeSelector, pair)))
      result.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        values => {
          val (_,schemaExpected,_,result) = values
          val (schema, shapeMap) = result
          (schemaExpected,schema) match {
            case (e: ShExSchema, s: ShExSchema) => {
              compareSchemas(e.schema, s.schema) match {
                case Left(e) => fail(s"Different $e\nInferred:\n${s.serialize("ShExC")}\nInferred shapeMap:\n${shapeMap.serialize("Compact")}")
                case Right(false) => fail(s"Compare schemas:\n${e.schema}\n${s.schema} returned false!")
                case Right(true) => info(s"Schemas are equal. Inferred: \n${s.serialize("ShExC")}\nInferred shapeMap:\n${shapeMap.serialize("Compact")}")
              }
            }
            case _ => fail(s"Should be ShEx shemas")
          }
        }
      )
    }
   }

  }

  private def compareSchemas(s1: Schema, s2: Schema): Either[String,Boolean] = {
    if (s1.shapesMap.keys != s2.shapesMap.keys) {
      Left(s"Different labels. labels1 = ${s1.shapesMap.keys}\nlabels2 = ${s2.shapesMap.keys}")
    } else {
      val zero: Either[String,Boolean] = Right(true)
      def compareLabelSE(e: Either[String,Boolean], pair: (ShapeLabel,ShapeExpr)): Either[String,Boolean] = for {
       b <- e
       x <- {
         val (lbl,se1) = pair
         s2.getShape(lbl) match {
           case Left(e) => Left(e)
           case Right(se2) => (se1,se2) match {
             case (sh1: Shape, sh2: Shape) => (sh1.expression, sh2.expression) match {
               case (Some(eo1: EachOf), Some(eo2: EachOf)) => compareEachOfs(eo1,eo2)
               case (Some(tc1: TripleConstraint), Some(tc2: TripleConstraint)) => compareTripleConstraints(tc1,tc2)
               case _ => Left(s"Should be EachOf or tripleConstraints: ${sh1.expression}, ${sh2.expression}")
             }
             case _ => Left(s"Should be shapes:\nse1=$se1\nse2=$se2")
           }
         }
       }
      } yield b
      s1.shapesMap.foldLeft(zero)(compareLabelSE)
    }
  }

  private def compareEachOfs(e1: EachOf, e2: EachOf): Either[String,Boolean] = {
    if (e1.expressions.length != e2.expressions.length) Left(s"Number of expressions in EachOf are different: $e1, $e2")
    else {
      val zero: Either[String,Boolean] = Right(true)
      def combine(b: Either[String,Boolean],x: TripleExpr): Either[String,Boolean] = x match {
        case tc: TripleConstraint =>
          findTripleConstraint(e2, tc.predicate) match {
            case Some(tc2) => compareTripleConstraints(tc, tc2)
            case None      => Left(s"Not found triple constraint in $e2 with predicate ${tc.predicate} which exists in $e1")
        }
        case _ => Left(s"Complex eachOf when it should contain only tripleConstraints: $x\nEachOf=$e1")
      }
      e1.expressions.foldLeft(zero)(combine)
    }
  }

  private def findTripleConstraint(eo: EachOf, p: IRI): Option[TripleConstraint] = {
   eo.expressions.collect {
      case tc: TripleConstraint if (tc.predicate == p) => tc
    }.headOption
  }
  private def compareTripleConstraints(tc1: TripleConstraint, tc2: TripleConstraint): Either[String,Boolean] = {
    if (tc1.predicate == tc2.predicate) {
      if (tc1.max == tc2.max) {
        if (tc1.min == tc2.min) {
          if (tc1.valueExpr == tc2.valueExpr) {
           Right(true)
          } else Left(s"Value expressions are different: \n${tc1.valueExpr}\n${tc2.valueExpr}")
        } else Left(s"Max cardinalities are different: ${tc1.min}, ${tc2.min}")
      } else Left(s"Max cardinalities are different: ${tc1.max}, ${tc2.max}")
    } else Left(s"Triple constraints are different because predicates are different: $tc1, $tc2")
  }
  }