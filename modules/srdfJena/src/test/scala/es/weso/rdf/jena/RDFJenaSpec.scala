package es.weso.rdf.jena

import org.scalatest.{EitherValues, FunSpec, Matchers}
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
import es.weso.rdf.PREFIXES._

class RDFJenaSpec
  extends FunSpec with JenaBased with Matchers with EitherValues {
  describe("Adding triples") {
    it("should be able to add a single triple with IRIs") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[Prefix, IRI] = Map(Prefix("") -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(RDFTriple(IRI("http://example.org#a"), IRI("http://example.org#b"), IRI("http://example.org#c"))))
      val m2 = str2model("""|@prefix : <http://example.org#> .
                            |:a :b :c .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf.model, m2)
    }
    it("should be able to add some triples with BNodes") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[Prefix, IRI] = Map(
        Prefix("") -> IRI("http://example.org#"),
        Prefix("foaf") -> IRI("http://foaf.org#"))
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(
        RDFTriple(IRI("http://example.org#a"), IRI("http://foaf.org#knows"), BNode("b" + 1)), RDFTriple(BNode("b" + 1), IRI("http://foaf.org#knows"), BNode("b" + 2)), RDFTriple(BNode("b" + 2), IRI("http://foaf.org#name"), StringLiteral("pepe"))))
      val m2 = str2model("""|@prefix : <http://example.org#> .
                            |@prefix foaf: <http://foaf.org#> .
                            |:a foaf:knows _:x .
                            |_:x foaf:knows _:y .
                            |_:y foaf:name "pepe" .
                            |""".stripMargin)

      shouldBeIsomorphic(rdf.model, m2)
    }

  }

  describe("Parsing other formats") {
    it("Should be able to parse NTriples") {
      val m1 = str2model("""|@prefix : <http://example.org#> .
                          |:a :b :c .
                          |""".stripMargin)
      val str_triples = "<http://example.org#a> <http://example.org#b> <http://example.org#c> ."
      val rdf: RDFAsJenaModel = RDFAsJenaModel(ModelFactory.createDefaultModel())
      val rdf2 = rdf.fromString(str_triples, "NTRIPLES").right.get
      val m2 = RDFAsJenaModel.extractModel(rdf2)
      shouldBeIsomorphic(m1, m2)
    }
  }

  describe("Querying RDF graphs") {
    it("Should be able to get iriObjects of some type") {
      val str = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.get
      val typeC = IRI("http://example.org#C")
      val triples = rdf.triplesWithType(typeC).right.value
      val a = IRI("http://example.org#a")
      val b = IRI("http://example.org#b")
      val t1 = RDFTriple(a, `rdf:type`, typeC)
      val t2 = RDFTriple(b, `rdf:type`, typeC)
      triples should be(Set(t1, t2))
    }

    it("Should be able to get subjects") {
      val str = """|@prefix : <http://example.org#> .
                   |:a a :C ; :p 1 .
                   |:b a :C, :D .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.value
      val a = IRI("http://example.org#a")
      val p = IRI("http://example.org#p")
      val typeC = IRI("http://example.org#C")
      val triples = rdf.triplesWithSubject(a).right.value

      val t1 = RDFTriple(a, `rdf:type`, typeC)
      val t2 = RDFTriple(a, p, IntegerLiteral(1,"1"))
      triples should be(Set(t1, t2))
    }

    it("Should be able to get subjects with xsd:date") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :date "25/10/2015"^^xsd:date .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.get
      val a = IRI("http://example.org#a")
      val date = IRI("http://example.org#date")
      val value = DatatypeLiteral("25/10/2015", IRI("http://www.w3.org/2001/XMLSchema#date"))
      val triples = rdf.triplesWithSubject(a).right.value
      val t1 = RDFTriple(a, date, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with xsd:integer") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age 15 .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = IntegerLiteral(15,"15")
      val triples = rdf.triplesWithSubject(a).right.value
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with datatype :xxx") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "15"^^:xxx .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = DatatypeLiteral("15", IRI("http://example.org#xxx"))
      val triples = rdf.triplesWithSubject(a).right.value
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

    it("Should be able to get subjects with lang literal") {
      val str = """|@prefix : <http://example.org#> .
                   |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                   |:a :age "hi"@en .
                   |""".stripMargin
      val rdf = RDFAsJenaModel.empty.fromString(str, "TURTLE").right.get
      val a = IRI("http://example.org#a")
      val age = IRI("http://example.org#age")
      val value = LangLiteral("hi", Lang("en"))
      val triples = rdf.triplesWithSubject(a).right.value
      val t1 = RDFTriple(a, age, value)
      triples should be(Set(t1))
    }

  }

  describe("hasClass") {
    it("check hasClass") {
      val ex = "http://example.org"
      val rdfStr = s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin
      val e = IRI(ex)
      val model = RDFAsJenaModel.empty.fromString(rdfStr).right.get
      val person1 = e + "person1"
      val teacher1 = e + "teacher1"
      val teacher2 = e + "teacher2"
      val dog1 = e + "dog1"
      val any = e + "any"
      val _Person = e + "Person"
      val _Teacher = e + "Teacher"
      val _UniversityTeacher = e + "UniversityTeacher"
      val _Dog = e + "Dog"
      val _Any = e + "Any"

      model.hasSHACLClass(person1, _Person).right.value should be(true)
      model.hasSHACLClass(person1, _Teacher).right.value should be(false)
      model.hasSHACLClass(person1, _UniversityTeacher).right.value should be(false)
      model.hasSHACLClass(person1, _Dog).right.value should be(false)
      model.hasSHACLClass(teacher1, _Person).right.value should be(true)
      model.hasSHACLClass(teacher1, _Teacher).right.value should be(true)
      model.hasSHACLClass(teacher1, _UniversityTeacher).right.value should be(false)
      model.hasSHACLClass(teacher1, _Dog).right.value should be(false)
      model.hasSHACLClass(teacher2, _Person).right.value should be(true)
      model.hasSHACLClass(teacher2, _Teacher).right.value should be(true)
      model.hasSHACLClass(teacher2, _UniversityTeacher).right.value should be(true)
      model.hasSHACLClass(teacher2, _Dog).right.value should be(false)
      model.hasSHACLClass(dog1, _Person).right.value should be(false)
      model.hasSHACLClass(dog1, _Teacher).right.value should be(false)
      model.hasSHACLClass(dog1, _UniversityTeacher).right.value should be(false)
      model.hasSHACLClass(dog1, _Dog).right.value should be(true)
      model.hasSHACLClass(any, _Dog).right.value should be(false)
      model.hasSHACLClass(any, _Any).right.value should be(false)
      model.hasSHACLClass(dog1, _Any).right.value should be(false)
    }
  }

  describe("getSHACLInstances") {
    it("getSHACLInstances") {
      val ex = "http://example.org"
      val rdfStr = s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin
      val e = IRI(ex)
      val model = RDFAsJenaModel.empty.fromString(rdfStr).right.get
      val person1 = e + "person1"
      val teacher1 = e + "teacher1"
      val teacher2 = e + "teacher2"
      val dog1 = e + "dog1"
      val _Person = e + "Person"
      val _Teacher = e + "Teacher"
      val _UniversityTeacher = e + "UniversityTeacher"
      val _Dog = e + "Dog"
      val _Any = e + "Any"

      model.getSHACLInstances(_Person).right.value should contain only (person1, teacher1, teacher2)
      model.getSHACLInstances(_Teacher).right.value should contain only (teacher1, teacher2)
      model.getSHACLInstances(_UniversityTeacher).right.value should contain only (teacher2)
      model.getSHACLInstances(_Dog).right.value should contain only (dog1)
      model.getSHACLInstances(_Any).right.value shouldBe empty
    }
  }

}
