package es.weso.rdf.jena

import org.apache.jena.query._
import es.weso.rdf.nodes._

object SPARQLQueries {

  def queryTriples() = {
    QueryFactory.create(
      s"""|construct {?x ?p ?y } where {
         |?x ?p ?y .
       |}
         |""".stripMargin)
  }

  def queryTriplesWithSubject(subj: IRI) = {
    val s = subj.str
    QueryFactory.create(
      s"""|construct {<${s}> ?p ?y } where {
         |<${s}> ?p ?y .
       |}
         |""".stripMargin)
  }

  def queryTriplesWithObject(obj: IRI) = {
    val s = obj.str
    QueryFactory.create(
      s"""|construct {?x ?p <${s}> } where {
         | ?x ?p <${s}> .
       |}
         |""".stripMargin)
  }

  def queryTriplesWithPredicate(obj: IRI) = {
    val s = obj.str
    QueryFactory.create(
      s"""|construct {?x <${s}> ?y } where {
          | ?x <${s}> ?y .
          |}
          |""".stripMargin)
  }

  def queryTriplesWithPredicateObject(p: IRI, o: IRI) = {
    QueryFactory.create(
      s"""|construct {?x <${p.str}> <${o.str}> } where {
          | ?x <${p.str}> <${o.str}> .
          |}
          |""".stripMargin)
  }

  lazy val findIRIs = QueryFactory.create(
    """|select ?x where {
         | ?x ?p ?y .
         | filter (isIRI(?x))
       |}
         |""".stripMargin)

  lazy val countStatements = QueryFactory.create(
    """|select (count(?s) as ?triples) where {
       | ?s ?p ?o .
       |}
       |""".stripMargin)

  lazy val findRDFTriples = QueryFactory.create(
    """|construct { ?x ?p ?y } where {
         | ?x ?p ?y .
       |}
         |""".stripMargin)

  lazy val findSubjects = QueryFactory.create(
    """|select ?x where {
         | ?x ?p ?y .
         | filter (isIRI(?x))
       |}
         |""".stripMargin)

  lazy val findPredicates = QueryFactory.create(
    """|select ?p where {
         | ?x ?p ?y .
         | filter (isIRI(?p))
       |}
         |""".stripMargin)

  lazy val findObjects = QueryFactory.create(
    """|select ?y where {
         | ?x ?p ?y .
         | filter (isIRI(?y))
       |}
         |""".stripMargin)

}