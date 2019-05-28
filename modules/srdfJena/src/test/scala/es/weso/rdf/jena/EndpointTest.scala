package es.weso.rdf.jena

import es.weso.rdf.nodes.{IRI, IntegerLiteral, RDFNode}
import org.scalatest.{FunSpec, Matchers}
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query.{Dataset, DatasetFactory, QueryExecutionFactory, QueryFactory, QuerySolution, ResultSet}
// import org.apache.jena.rdf.model.ModelFactory

class EndpointTest
  extends FunSpec
  with JenaBased
  with Matchers {

  describe("Checking endpoint") {
    val dataset = "ds"
    val portNumber: Int = 5678
    val endpoint = s"http://localhost:${portNumber}/${dataset}/sparql"
    val ex = IRI("http://example.org/")
    //    val endpointUpdate = s"http://host:3330/${dataset}/update"

    it("should be able to run a query over an endpoint") {
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
    }

    def shouldQueryData(data: String, queryStr: String, expected: List[Map[String, RDFNode]]): Unit = {
      val eitherRdf = for {
        rdf <- RDFAsJenaModel.fromChars(data,"Turtle")
      } yield rdf
      eitherRdf.fold(
        e => fail(s"Error: $e"),
        rdf => {
          val ds = DatasetFactory.createTxnMem
          ds.setDefaultModel(rdf.model)
          val server = FusekiServer.create.
            port(portNumber).
            add(s"/${dataset}", ds
            ).build
          server.start
          val query = QueryFactory.create(queryStr)
          val rs = QueryExecutionFactory.sparqlService(endpoint, query).execSelect()
          server.stop

          val eitherResultMap = JenaMapper.resultSet2Map(rs)
          eitherResultMap.fold(e => fail(s"Error: $e"), rm => {
            println(s"ResultMap: $rm")
            shouldCompareListMaps(rm, expected)
          })
        }
      )
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
