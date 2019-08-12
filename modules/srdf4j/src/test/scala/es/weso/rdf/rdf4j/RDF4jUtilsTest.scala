package es.weso.rdf.rdf4j

import java.io.InputStream

import es.weso.rdf.nodes._
import es.weso.rdf.path._
import org.apache.commons.io.IOUtils
import org.scalatest._
import org.eclipse.rdf4j.rio.RDFFormat
import org.eclipse.rdf4j.rio.Rio
import RDF4jUtils._

class RDF4jUtilsTest extends FunSpec with Matchers with EitherValues with OptionValues {

  describe(s"subjectsFromPath") {
    it(s"Should check happy path") {
      val input: InputStream = IOUtils.toInputStream(
        """|prefix : <http://example.org/>
           |:x :p 1, :y .
           |:z :q :y .
           |
      """.stripMargin
      )
      val model = Rio.parse(input, "", RDFFormat.TURTLE)
      val prefix = IRI("http://example.org/")
      val x = prefix + "x"
      val y = prefix + "y"
      val z = prefix + "z"
      val p = prefix + "p"
      val q = prefix + "q"
      val path = AlternativePath(Seq(PredicatePath(p),PredicatePath(q)))
      val nodes = subjectsWithPath(y, path, model)
      nodes should contain only(x, z)
    }
  }

  describe(s"objectsFromPath") {
    it(s"Should check happy path") {
      val input: InputStream = IOUtils.toInputStream(
        """|prefix : <http://example.org/>
           |:x :p :a, :y .
           |:y :p :z .
           |:x :q :r .
      """.stripMargin
      )
      val model = Rio.parse(input, "", RDFFormat.TURTLE)
      val prefix = IRI("http://example.org/")
      val x = prefix + "x"
      val a = prefix + "a"
      val y = prefix + "y"
      val z = prefix + "z"
      val r = prefix + "r"
      val p = prefix + "p"
      val q = prefix + "q"
      // p+ | q
      val path = AlternativePath(
        Seq(OneOrMorePath(PredicatePath(p)),
        PredicatePath(q))
      )
      val nodes = objectsWithPath(x, path, model)
      nodes should contain only(y, a, z, r)
    }
  }
}