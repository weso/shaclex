package es.weso.rdf.jena

import java.io.{ByteArrayInputStream, InputStream, StringReader}
import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Paths

import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import org.apache.jena.graph.Graph
import org.apache.jena.riot._
import org.apache.jena.riot.RDFLanguages.shortnameToLang
import org.apache.jena.riot.system.{IRIResolver, RiotLib, StreamRDFLib}
import org.apache.jena.sparql.core.{DatasetGraph, DatasetGraphFactory}
import org.apache.jena.sparql.graph.GraphFactory

import util._

class RDFAsJenaModelTest
  extends FunSpec
  with JenaBased
  with Matchers {

  describe("Checking base") {
    val conf: Config = ConfigFactory.load()
    val shaclFolder = conf.getString("shaclCore")
    val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString
    println(s"ShaclFolder file...${shaclFolderURI}")

    it("should be able to parse RDF with relative URIs and base") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[Prefix, IRI] = Map(Prefix("") -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(RDFTriple(
        IRI("http://example.org#a"),
        IRI("http://example.org#b"),
        IRI(shaclFolderURI + "c"))))

      val str =
        """|@prefix : <http://example.org#> .
                   |:a :b <c> .
                   |""".stripMargin
      RDFAsJenaModel.fromChars(str, "TURTLE", Some(shaclFolderURI)) match {
        case Success(m2) => shouldBeIsomorphic(rdf.model, m2.model)
        case Failure(e) => fail(s"Error $e\n$str")
      }

    }
    it("should be able to parse RDF with relative URIs") {
      val emptyModel = ModelFactory.createDefaultModel
      println(s"Base resolver: ${IRIResolver.chooseBaseURI()}")
      val resolver = IRIResolver.createNoResolve()
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      rdf.addTriples(Set(RDFTriple(
        IRI("a"),
        IRI("b"),
        IntegerLiteral(1))
      ))
      val str =
        """|<a> <b> 1 .
                   |""".stripMargin
      val m = ModelFactory.createDefaultModel
      val str_reader = new StringReader(str.toString)
      val stream : InputStream = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8.name()))
      val sysRiot = SysRIOT.setStrictMode(true)
      println(s"SysRIOT strict: ${SysRIOT.AbsURINoNormalization}, ${SysRIOT.strictMode}")
//      m.read(stream,"","TURTLE")
      val parser = RDFParser.create().fromString(str).lang(shortnameToLang("Turtle"))
        // .resolveURIs(false) // RDFParserBuilder
//      parser.resolveURIs = false;
      val g: Graph = GraphFactory.createDefaultGraph()
      val streamRDF = StreamRDFLib.graph(g)
      streamRDF.start
      parser.parse(streamRDF)
      streamRDF.finish
      println("Graph:")
      println(g)

      RDFDataMgr.read(m, str_reader, "", shortnameToLang("Turtle"))
      shouldBeIsomorphic(rdf.model, m)
/*      RDFAsJenaModel.fromChars(str, "TURTLE", Some("")) match {
        case Success(m2) => shouldBeIsomorphic(rdf.model, m2.model)
        case Failure(e) => fail(s"Error $e\n$str")
      } */
    }
  }
}

