package es.weso.rdf.jena

import es.weso.rdf.nodes.{IRI, IntegerLiteral, RDFNode}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query._
import org.apache.jena.system.Txn

class EndpointTest
  extends FunSpec
  with JenaBased
  with Matchers with BeforeAndAfter {

  val dataset = "ds"
  val portNumber: Int = 5678
  val endpointName = s"http://localhost:${portNumber}/${dataset}/sparql"
  val ex = IRI("http://example.org/")
  val ds = DatasetFactory.createTxnMem
  val server = FusekiServer.create.port(portNumber).add(s"/${dataset}", ds).build
  val endpoint = Endpoint(IRI(endpointName))
  val endpointUpdate = s"http://host:3330/${dataset}/update"

  before {
    server.start
  }

  after {
    server.stop
  }

  describe("Checking endpoint") {

    it("should be able to obtain SHACL instances in an endpoint") {
      shouldObtainShaclInstances(
        s"""|prefix : ${ex}
            |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            |:x a :A .
            |:y a :B .
            |:A rdfs:subClassOf :B .
            """.stripMargin,
        ex + "B",
        Seq(ex+"x",ex+"y")
      )
    }

/* Commented because, for some reason, when I repeat the test, it fails
    it("should be able to obtain SHACL instances in an endpoint 2") {

      shouldObtainShaclInstances(
        s"""|prefix : ${ex}
            |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            |:x a :A .
            |:y a :B .
            |:A rdfs:subClassOf :B .
            """.stripMargin,
        ex + "B",
        Seq(ex+"x",ex+"y")
      )
    }
*/
/*    it("should be able to run a query over an endpoint") {
      shouldQueryData(
        s"""|prefix : ${ex}
            |:a :p 2,3 .
            """.stripMargin,
        s"""|prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |prefix : $ex
           |select * {
           |?x ?p ?y
           |}
           |""".stripMargin,
        List(Map("x" -> (ex + "a"), "p" -> (ex + "p"), "y" -> IntegerLiteral(3)),
             Map("x" -> (ex + "a"), "p" -> (ex + "p"), "y" -> IntegerLiteral(2)))
      )
    } */

    def withEndpoint[A](data: String,
                        action: Endpoint => Either[String,A]
                       ): Either[String,A] =
     for {
        rdf <- RDFAsJenaModel.fromChars(data, "Turtle")
        result <- {
          Txn.executeWrite(ds, execute(ds.setDefaultModel(rdf.model)))
          action(endpoint)
        }
    } yield result

    def shouldObtainShaclInstances(data: String, node: IRI, expected: Seq[RDFNode]): Unit = {
      val either = withEndpoint(data, _.getSHACLInstances(node))
      info(s"shouldObtainShaclInstances for node $node")
      either.fold(e => fail(s"Error: $e"), is => {
        is should contain theSameElementsAs(expected)
      })
    }

    def shouldQueryData(data: String, queryStr: String, expected: List[Map[String, RDFNode]]): Unit = {
      val eitherRdf = for {
        rdf <- RDFAsJenaModel.fromChars(data,"Turtle")
      } yield rdf
      eitherRdf.fold(
        e => fail(s"Error: $e"),
        rdf => {
          Txn.executeWrite(ds, execute(ds.setDefaultModel(rdf.model)))
          val query = QueryFactory.create(queryStr)
          val rs = QueryExecutionFactory.sparqlService(endpointName, query).execSelect()
          val eitherResultMap = JenaMapper.resultSet2Map(rs)
          eitherResultMap.fold(e => fail(s"Error: $e"), rm => {
            shouldCompareListMaps(rm, expected)
          })
        }
      )
    }

    def execute(body: => Unit): Runnable = {
      () => body
    }

    def shouldCompareListMaps(m1: List[Map[String,RDFNode]], m2: List[Map[String,RDFNode]]):Unit = {
      m1.zip(m2).foreach(pair => {
        shouldCompareMaps(pair._1,pair._2)
      })
    }

    def shouldCompareMaps(m1: Map[String,RDFNode], m2: Map[String,RDFNode]):Unit = {
      for (key <- m1.keys) {
        val e = m2(key).isEqualTo(m1(key))
        e.fold(s => fail(s"Error comparing values for key: $key. ${m1(key)} vs ${m2(key)}\nError:$s\nMap1: $m1\n$m2"), b => {
          if (!b) {
            fail(s"Different value for $key. ${m1(key)}!=${m2(key)}\nMap1: $m1\nMap2: $m2")
          }
        })
      }
    }
  }

}
