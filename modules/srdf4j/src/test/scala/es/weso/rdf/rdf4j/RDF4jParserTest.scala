package es.weso.rdf.rdf4j

import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes._
import org.scalatest._

import scala.util._

class RDF4jParserTest extends FunSpec with Matchers with EitherValues with OptionValues {

  describe("RDF4jParser") {
    it(s"Should parse simple Turtle string") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val mayberdf = RDFAsRDF4jModel.fromChars(str,"Turtle",None)
      mayberdf match {
        case Left(str) => fail(s"Error parsing: $str")
        case Right(rdf) => rdf.getNumberOfStatements().right.value should be(2)
      }
    }
  }

  describe(s"RDF4j API as SRDF") {
    it(s"Should be able to parse prefix maps") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val mayberdf = RDFAsRDF4jModel.fromChars(str,"Turtle",None)
      mayberdf match {
        case Left(str) => fail(s"Error parsing: $str")
        case Right(rdf) => {
          val pm = rdf.getPrefixMap()
          pm.getIRI("").value should be(IRI("http://example.org/"))
        }
      }
    }

    it(s"Should be able to extend the prefix map") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val mayberdf = RDFAsRDF4jModel.fromChars(str,"Turtle",None)
      mayberdf match {
        case Left(str) => fail(s"Error parsing: $str")
        case Right(rdf) => {
          rdf.addPrefixMap(PrefixMap(Map(Prefix("kiko") -> IRI("http://kiko.org"))))
          rdf.getPrefixMap.getIRI("kiko").value should be(IRI("http://kiko.org"))
          rdf.getPrefixMap.getIRI("pepe") should be(None)
        }
      }
    }

    it(s"Should be able to get subjects") {
      val str =
        """prefix : <http://example.org/>
          |:x :p :y .
          |:y a 1 .
        """.stripMargin
      val mayberdf = RDFAsRDF4jModel.fromChars(str,"Turtle",None)
      mayberdf match {
        case Left(str) => fail(s"Error parsing: $str")
        case Right(rdf) => {
          val ts = rdf.triplesWithSubject(IRI("http://example.org/x")).right.value
          ts.size should be(1)
        }
      }
    }

  }
}
