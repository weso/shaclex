package es.weso.rdf.jena

import org.apache.jena.query._
import es.weso.rdf.nodes._
import es.weso.rdf.path.SHACLPath

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

  def queryTriplesWithSubjectPredicate(subj: IRI, pred: IRI) = {
    val s = subj.str
    val p = pred.str
    val query = QueryFactory.create(
      s"""|construct {<${s}> <${p}> ?y } where {
          |<${s}> <${p}> ?y .
          |}
          |""".stripMargin)
    println(s"Running query: $query")
    query
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

  lazy val findClasses = QueryFactory.create(
    """|select ?c where {
       | ?s a ?c .
       |} LIMIT 100
       |""".stripMargin)

  lazy val countStatements = QueryFactory.create(
    """|select (count(*) as ?c) where {
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
       |}""".stripMargin)

  lazy val findObjects = QueryFactory.create(
    """|select ?y where {
         | ?x ?p ?y .
         | filter (isIRI(?y))
       |}
         |""".stripMargin)

  def queryShaclInstances(obj: IRI) = {
    val s = obj.str
    QueryFactory.create(
      s"""|prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |
          |select ?x where {
          | ?x rdf:type/rdfs:subClassOf* <${s}> .
          |}
          |""".stripMargin)
  }

  def queryHasShaclClass(node: IRI, c: IRI) = {
    QueryFactory.create(
      s"""|prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |
          |ask {
          | <${node.str}> rdf:type/rdfs:subClassOf* <${c.str}> .
          |}
          |""".stripMargin)
  }

  def queryPath(p: SHACLPath) = {
    QueryFactory.create(
      s"""|select ?x ?y {
          | ?x ${JenaMapper.shaclPath2JenaPath(p)} ?y .
          |}
          |""".stripMargin)
  }

  def querySubjectsWithPath(iri: IRI, p: SHACLPath) = {
    QueryFactory.create(
      s"""|select ?x {
          | ?x ${JenaMapper.shaclPath2JenaPath(p)} <${iri.str}> .
          |}
          |""".stripMargin)
  }

  def queryObjectsWithPath(iri: IRI, p: SHACLPath) = {
    QueryFactory.create(
      s"""|select ?x {
          | <${iri.str}> ${JenaMapper.shaclPath2JenaPath(p)} ?x .
          |}
          |""".stripMargin)
  }

}