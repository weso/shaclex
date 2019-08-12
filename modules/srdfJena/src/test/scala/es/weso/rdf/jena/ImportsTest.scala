package es.weso.rdf.jena

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.nodes._
import org.scalatest.{EitherValues, FunSpec, Matchers}

class ImportsTest extends FunSpec with JenaBased with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val rdfFolderStr = conf.getString("rdfFolder")
  val rdfFolder = IRI(Paths.get(rdfFolderStr).normalize.toUri.toString)

  describe("Merge test") {
    it(s"Should read merged file") {
    val r = for {
      rdf1 <- RDFAsJenaModel.fromIRI(rdfFolder + "/merged.ttl")
      iris <- rdf1.iris
    } yield (rdf1,iris)

    r.fold(e => fail(s"Error: $e"), t => {
      val (rdf,iris) = t
      info(s"RDF read: ${iris}")
      iris.size should be(1)
    })
    }

    it(s"Should merge files") {
      val r = for {
        rdf1 <- RDFAsJenaModel.fromIRI(rdfFolder + "/m1.ttl")
        rdf2 <- RDFAsJenaModel.fromIRI(rdfFolder + "/m2.ttl")
        merged <- rdf1.merge(rdf2.normalizeBNodes)
        mergedFromFile <- RDFAsJenaModel.fromIRI(rdfFolder + "/merged.ttl")
        b <- merged.isIsomorphicWith(mergedFromFile)
      } yield {
        println(s"rdf1: ${rdf1.serialize("N-TRIPLES")}")
        println(s"rdf2: ${rdf2.serialize("N-TRIPLES")}")
        println(s"merged: ${merged.serialize("N-TRIPLES")}")
        println(s"mergedExpected: ${mergedFromFile.serialize("N-TRIPLES")}")
        (merged,mergedFromFile,b)
      }

      r.fold(e => fail(s"Error: $e"), values => {
        val (_, _,b) = values
        b should be(true)
      })
    }
  }

  describe("Imports test") {
    it(s"Should extend with imports") {
     val r = for {
      rdf1     <- RDFAsJenaModel.fromIRI(rdfFolder + "/testImport.ttl")
      extended <- rdf1.extendImports()
      } yield (rdf1,extended)
      r.fold(e => fail(s"Error: $e"), values => {
        val (_,extended) = values
        val x = IRI("http://example.org/x")
        val p = IRI("http://example.org/p")
        extended.triplesWithSubjectPredicate(x,p).right.value.size should be(2)
//        info(s"Extended: ${extended.serialize("Turtle")}")
      })
    }

    it(s"Should handle loops") {
      val r = for {
        rdf1     <- RDFAsJenaModel.fromIRI(rdfFolder + "/testImportWithLoop.ttl")
        extended <- rdf1.extendImports()
        ts <- rdf1.rdfTriples
        tse <- extended.rdfTriples
      } yield (rdf1,extended,ts,tse)
      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf1,extended,ts,tse) = values
        ts.size should be(tse.size)
      })
    }

    ignore(s"Should handle external IRIs") {
      val r = for {
        rdf1     <- RDFAsJenaModel.fromChars(
          """|prefix : <http://example.org/>
            |<> owl:imports <...some IRI?...>
            |""".stripMargin,"Turtle",None)
        extended <- rdf1.extendImports()
        ts1 <- rdf1.rdfTriples()
        tse <- extended.rdfTriples()
      } yield (rdf1,extended,ts1,tse)
      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf1,extended,ts1,tse) = values
        ts1.size should be(tse.size)
      })
    }

  }
}

