package es.weso.shacl

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import org.scalatest._

class ImportTest extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolderStr = conf.getString("shaclTests")
  val shaclFolder = IRI(Paths.get(shaclFolderStr).normalize.toUri.toString)

  describe("import") {
    it(s"Validates a shape that imports another one") {
      val r = for {
        rdf    <- RDFAsJenaModel.fromIRI(shaclFolder + "imports/import.ttl")
        schema <- RDF2Shacl.getShacl(rdf)
        result <- Validator.validate(schema, rdf)
      } yield result

      r.fold(
        e => fail(s"Error reading: $e"),
        pair => {
        val (typing, ok) = pair
        val alice = IRI("http://example.org/alice")
        val bob = IRI("http://example.org/bob")
        val person = IRI("http://example.org/Person")
        val hasName = IRI("http://example.org/hasName")
        typing.getFailedValues(alice).map(_.id) should contain theSameElementsAs(List())
        typing.getFailedValues(bob).map(_.id) should contain theSameElementsAs(List(person,hasName))
      })
    }

    it(s"Validates a shape that imports another one with a loop") {
      val r = for {
        rdf    <- RDFAsJenaModel.fromIRI(shaclFolder + "imports/importWithLoop.ttl")
        schema <- RDF2Shacl.getShacl(rdf)
        result <- Validator.validate(schema, rdf)
      } yield result

      r.fold(
        e => fail(s"Error reading: $e"),
        pair => {
          val (typing, ok) = pair
          val alice = IRI("http://example.org/alice")
          val bob = IRI("http://example.org/bob")
          val person = IRI("http://example.org/Person")
          val hasName = IRI("http://example.org/hasName")
          typing.getFailedValues(alice).map(_.id) should contain theSameElementsAs(List())
          typing.getFailedValues(bob).map(_.id) should contain theSameElementsAs(List(person,hasName))
        })
    }
  }
}
