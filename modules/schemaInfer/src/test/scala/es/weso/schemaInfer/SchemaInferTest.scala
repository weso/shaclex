package es.weso.schemaInfer
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.rdf.parser.RDFParser
import es.weso.schema.{Schemas, ShExSchema}
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._
import org.scalatest.{FunSpec, Matchers}

class SchemaInterTest extends FunSpec with Matchers with RDFParser {

  describe(s"Schema Infer") {

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

    def checkSchemaInfer(rdfStr: String, expectedStr: String, nodeSelectorStr: String, label: String): Unit = {
    it(s"Should infer a ShEx schema: $rdfStr for node $nodeSelectorStr and obtain $expectedStr") {
      val result = for {
       rdf <- RDFAsJenaModel.fromChars(rdfStr, "TURTLE")
       schemaExpected <- Schemas.fromString(expectedStr,"ShExC","ShEx")
       nodeSelector <- NodeSelector.fromString(nodeSelectorStr, None,rdf.getPrefixMap)
       schema <- SchemaInfer.infer(rdf, nodeSelector, "ShEx", IRI("S"))
      } yield (rdf, schemaExpected, nodeSelector, schema)
      result.fold(
        e => fail(s"Error: $e"),
        values => {
          val (rdf,schemaExpected,nodeSelector,schema) = values
          (schemaExpected,schema) match {
            case (e: ShExSchema, s: ShExSchema) => {
              compareSchemas(e.schema, s.schema) match {
                case Left(e) => fail(s"Different $e\nInferred:\n${s.serialize("ShExC")}")
                case Right(false) => fail(s"Compare schemas returned false!")
                case Right(true) => info(s"Schemas are equal. Inferred: \n${s.serialize("ShExC")}")
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
    if (s1.localShapesMap.keys != s2.localShapesMap.keys) {
      Left(s"Different labels. labels1 = ${s1.localShapesMap.keys}\nlabels2 = ${s2.localShapesMap.keys}")
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
      s1.localShapesMap.foldLeft(zero)(compareLabelSE)
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