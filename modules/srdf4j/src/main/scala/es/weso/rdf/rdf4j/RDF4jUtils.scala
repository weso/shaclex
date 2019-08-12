package es.weso.rdf.rdf4j

import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.path.{PredicatePath, SHACLPath, SequencePath, ZeroOrMorePath}
import org.eclipse.rdf4j.model.Model
import org.eclipse.rdf4j.repository.sail.SailRepository
import org.eclipse.rdf4j.sail.memory.MemoryStore
import cats.implicits._
import RDF4jMapper._
import es.weso.rdf.PREFIXES._

import scala.collection.mutable.ListBuffer


object RDF4jUtils {

  def subjectsWithPath(obj: RDFNode, path: SHACLPath, model: Model): Seq[RDFNode] = {
    // Build the following query:
    // SELECT ?sub { ?sub ?path ?obj }
    val repo = new SailRepository(new MemoryStore)
    repo.initialize
    val con = repo.getConnection
    con.add(model)
    val queryStr =
      s"""|SELECT ?x {
          |?x ${path.show} ${obj.show}
          |}""".stripMargin
    val query = con.prepareTupleQuery(queryStr)
    val result = query.evaluate
    val rs: ListBuffer[RDFNode] = ListBuffer()
    while (result.hasNext) {
      val bindingSet = result.next
      val x = bindingSet.getValue("x")
      val node = value2RDFNode(x)
      rs += node
    }
    rs.toList
  }


  def objectsWithPath(subj: RDFNode, path: SHACLPath, model: Model): Seq[RDFNode] = {
    // Build the following query:
    // SELECT ?obj { ?n ?path ?obj }
    val repo = new SailRepository(new MemoryStore)
    repo.initialize
    val con = repo.getConnection
    con.add(model)
    val queryStr =
      s"""|SELECT ?x {
          |${subj.show} ${path.show} ?x
          |}""".stripMargin
    val query = con.prepareTupleQuery(queryStr)
    val result = query.evaluate
    val rs: ListBuffer[RDFNode] = ListBuffer()
    while (result.hasNext) {
      val bindingSet = result.next
      val x = bindingSet.getValue("x")
      val node = value2RDFNode(x)
      rs += node
    }
    rs.toList
  }

  def getSHACLInstances(cls: RDFNode, model: Model): Seq[RDFNode] = {
    // Build the following query:
    // SELECT ?x { ?x rdf:type/rdfs:subClassOf* ?x }
    val repo = new SailRepository(new MemoryStore)
    repo.initialize
    val con = repo.getConnection
    con.add(model)
    val path: SHACLPath = SequencePath(Seq(PredicatePath(`rdf:type`), ZeroOrMorePath(PredicatePath(`rdfs:subClassOf`))))
    val queryStr =
      s"""|SELECT ?x {
          |?x ${path.show} ${cls.show}
          |}""".stripMargin
    val query = con.prepareTupleQuery(queryStr)
    val result = query.evaluate
    val rs: ListBuffer[RDFNode] = ListBuffer()
    while (result.hasNext) {
      val bindingSet = result.next
      val x = bindingSet.getValue("x")
      val node = value2RDFNode(x)
      rs += node
    }
    rs.toList
  }

}