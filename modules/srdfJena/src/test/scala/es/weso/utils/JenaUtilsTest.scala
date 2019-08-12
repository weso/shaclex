package es.weso.utils

import es.weso.rdf.jena.JenaMapper
import es.weso.rdf.nodes.IRI
import es.weso.rdf.path.PredicatePath
import org.apache.jena.sparql.path.{P_Link, P_OneOrMoreN}
import org.scalatest._

class JenaUtilsTest extends FunSpec with Matchers with EitherValues {

  describe("hasClass") {
    it("check hasClass") {
      val ex = "http://example.org/"
      val rdfStr =s"""|@prefix : <$ex> .
                   |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                   |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>. 
                   |:person1 a :Person .
                   |:teacher1 a :Teacher .
                   |:teacher2 a :UniversityTeacher .
                   |:Teacher rdfs:subClassOf :Person .
                   |:UniversityTeacher rdfs:subClassOf :Teacher .
                   |:dog1 a :Dog .""".stripMargin


      JenaUtils.parseFromString(rdfStr) match {
        case Right(model) => {
          val person1 = model.createResource(ex + "person1")
          val teacher1 = model.createResource(ex + "teacher1")
          val teacher2 = model.createResource(ex + "teacher2")
          val dog1 = model.createResource(ex + "dog1")
          val any = model.createResource(ex + "any")
          val _Person = model.createResource(ex + "Person")
          val _Teacher = model.createResource(ex + "Teacher")
          val _UniversityTeacher = model.createResource(ex + "UniversityTeacher")
          val _Dog = model.createResource(ex + "Dog")
          val _Any = model.createResource(ex + "Any")

          JenaUtils.hasClass(person1, _Person, model) should be(true)
          JenaUtils.hasClass(person1, _Teacher, model) should be(false)
          JenaUtils.hasClass(person1, _UniversityTeacher, model) should be(false)
          JenaUtils.hasClass(person1, _Dog, model) should be(false)
          JenaUtils.hasClass(teacher1, _Person, model) should be(true)
          JenaUtils.hasClass(teacher1, _Teacher, model) should be(true)
          JenaUtils.hasClass(teacher1, _UniversityTeacher, model) should be(false)
          JenaUtils.hasClass(teacher1, _Dog, model) should be(false)
          JenaUtils.hasClass(teacher2, _Person, model) should be(true)
          JenaUtils.hasClass(teacher2, _Teacher, model) should be(true)
          JenaUtils.hasClass(teacher2, _UniversityTeacher, model) should be(true)
          JenaUtils.hasClass(teacher2, _Dog, model) should be(false)
          JenaUtils.hasClass(dog1, _Person, model) should be(false)
          JenaUtils.hasClass(dog1, _Teacher, model) should be(false)
          JenaUtils.hasClass(dog1, _UniversityTeacher, model) should be(false)
          JenaUtils.hasClass(dog1, _Dog, model) should be(true)
          JenaUtils.hasClass(any, _Dog, model) should be(false)
          JenaUtils.hasClass(any, _Any, model) should be(false)
          JenaUtils.hasClass(dog1, _Any, model) should be(false)
        }
        case Left(msg) => fail(msg)
      }
    }

    it(s"Check hasClass with blank nodes") {
      val rdfStr =
        s"""|@prefix : <http://example.org/>
            |_:x a :A, :B, _:C ;
            |    :x 1 . # To identify it as _:x
            |_:y a :B, _:C ;
            |    :y 1 .
            |:z a :B .
            |_:C :c 1 .
         """.stripMargin
      val model = JenaUtils.parseFromString(rdfStr).right.value
      val ex = IRI.fromString("http://example.org/").right.value
      val px = JenaMapper.path2JenaPath(PredicatePath(ex + "x"), model, None).right.value
      val py = JenaMapper.path2JenaPath(PredicatePath(ex + "y"), model, None).right.value
      val pc = JenaMapper.path2JenaPath(PredicatePath(ex + "c"), model, None).right.value
      val bx = JenaUtils.getNodesFromPath(px, model).head._1
      val by = JenaUtils.getNodesFromPath(py, model).head._1
      val a = JenaMapper.rdfNode2JenaNode(ex+"A", model, None)
      val b = JenaMapper.rdfNode2JenaNode(ex+"B", model, None)
      val z = JenaMapper.rdfNode2JenaNode(ex+"z", model, None)
      val bc = JenaUtils.getNodesFromPath(pc, model).head._1
      JenaUtils.hasClass(bx,a,model) should be(true)
      JenaUtils.hasClass(bx,b,model) should be(true)
      JenaUtils.hasClass(by,a,model) should be(false)
      JenaUtils.hasClass(by,b,model) should be(true)
      JenaUtils.hasClass(bx,bc,model) should be(true)
      JenaUtils.hasClass(by,bc,model) should be(true)
      JenaUtils.hasClass(z,bc,model) should be(false)
    }
  }

  describe(s"HasSHACLInstances") {
    it(s"hasSHACLinstances should obtain SHACL instances") {
      val ex = "http://example.org/"
      val rdfStr = s"""|@prefix : <$ex> .
                       |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.
                       |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
                       |:person1 a :Person .
                       |:teacher1 a :Teacher .
                       |:teacher2 a :UniversityTeacher .
                       |:Teacher rdfs:subClassOf :Person .
                       |:UniversityTeacher rdfs:subClassOf :Teacher .
                       |:dog1 a :Dog .""".stripMargin

      val model = JenaUtils.parseFromString(rdfStr).right.value
      val person1 = model.createResource(ex + "person1")
      val teacher1 = model.createResource(ex + "teacher1")
      val teacher2 = model.createResource(ex + "teacher2")
      val dog1 = model.createResource(ex + "dog1")
      val _Person = model.createResource(ex + "Person")
      val _Teacher = model.createResource(ex + "Teacher")
      val _UniversityTeacher = model.createResource(ex + "UniversityTeacher")
      val _Dog = model.createResource(ex + "Dog")
      val _Any = model.createResource(ex +"Any")

      JenaUtils.getSHACLInstances(_Person, model).right.value should contain only (person1, teacher1, teacher2)
      JenaUtils.getSHACLInstances(_Teacher, model).right.value should contain only (teacher1, teacher2)
      JenaUtils.getSHACLInstances(_UniversityTeacher, model).right.value should contain only (teacher2)
      JenaUtils.getSHACLInstances(_Dog, model).right.value should contain only (dog1)
      JenaUtils.getSHACLInstances(_Any, model).right.value shouldBe empty
    }
  }

  describe("getValuesFromPath") {
    it("Validates parent") {
      val ex = "http://example.org#"
      val rdfStr =s"""|@prefix : <$ex> .
                    |:homer :parent :bart .
                    |:homer :parent :lisa .
                    |:homer :parent :maggie .
                    |:abraham :parent :homer .
                    |:abraham :parent :herb .
                    |""".stripMargin
      JenaUtils.parseFromString(rdfStr) match {
        case Right(model) => {
          val abraham = model.createResource(ex + "abraham")
          val homer = model.createResource(ex + "homer")
          val herb = model.createResource(ex + "herb")
          val bart = model.createResource(ex + "bart")
          val lisa =  model.createResource(ex + "lisa")
          val maggie = model.createResource(ex + "maggie")
          val parent = model.createResource(ex + "parent")
          val parent1 = new P_Link(parent.asNode)
          val parentPlus = new P_OneOrMoreN(parent1)
          JenaUtils.objectsFromPath(abraham, parent1, model) should contain only (herb, homer)
          JenaUtils.objectsFromPath(abraham, parentPlus, model) should contain only (bart, lisa, maggie, herb, homer
          )
        }
        case Left(msg) => fail(msg)
      }
    }
  }
}

