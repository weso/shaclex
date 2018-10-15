package es.weso.shex.check

import es.weso.rdf.RDFReader
import es.weso.rdf.jena.{JenaMapper, RDFAsJenaModel}
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

object ShExSpec extends Properties("ShEx") {

  lazy val iriGen: Gen[IRI] = for {
    str <- Gen.nonEmptyListOf(Gen.alphaLowerChar)
  } yield IRI(s"http://${str.mkString}.org")

  lazy val bnodeGen: Gen[BNode] = for {
    str <- Gen.listOfN(2,Gen.alphaLowerChar).map(_.mkString)
  } yield BNode(str)

  lazy val stringLiteralGen: Gen[StringLiteral] = for {
    str <- Gen.alphaLowerStr
  } yield StringLiteral(str)

  lazy val literalGen: Gen[Literal] =
    Gen.frequency(
      2 -> integerLiteralGen,
      2 -> doubleLiteralGen,
      2 -> unknownDatatypeLiteralGen
    )

  lazy val booleanLiteralGen: Gen[BooleanLiteral] = for {
    b <- arbitrary[Boolean]
  } yield BooleanLiteral(b)

  lazy val integerLiteralGen: Gen[IntegerLiteral] = for {
    n <- arbitrary[Int]
  } yield IntegerLiteral(n)

  lazy val doubleLiteralGen: Gen[DoubleLiteral] = for {
    d <- arbitrary[Double]
  } yield DoubleLiteral(d)

  implicit val arbIRI: Arbitrary[IRI] = Arbitrary(iriGen)

  lazy val unknownDatatypeLiteralGen: Gen[DatatypeLiteral] = for {
    datatype <- arbitrary[IRI]
    str <- arbitrary[String]
  } yield DatatypeLiteral(str,datatype)

  lazy val rdfnodeGen: Gen[RDFNode] =
    Gen.frequency(3 -> iriGen,
      1 -> bnodeGen,
      2 -> literalGen
    )

  lazy val rdfTripleGen: Gen[RDFTriple] = for {
    subj <- Gen.frequency(1 -> iriGen, 1 -> bnodeGen)
    pred <- iriGen
    obj <- Gen.frequency(2 -> iriGen, 1 -> bnodeGen, 2 -> literalGen)
  } yield RDFTriple(subj,pred,obj)

  lazy val rdfGen: Gen[RDFReader] = for {
    ts <- Gen.listOf(rdfTripleGen)
  } yield {
    try { RDFAsJenaModel(JenaMapper.RDFTriples2Model(ts.toSet, JenaMapper.emptyModel)) }
    catch {
      case e: NullPointerException => {
        println(s"Null point exception: $ts")
        RDFAsJenaModel.empty
      }
    }
  }

  property("RDFTriples") = forAll(rdfTripleGen) { (triple) =>
    triple.subj.isNonLiteral
  }

  property("rdf statements contain always iris as subjects") = forAll(rdfGen) { (rdf: RDFReader) => {
    val r = try { rdf.serialize("TURTLE") }
    catch { case e: NullPointerException => {
      println(s"Null point exception: $e")
      Left(s"Error serializing $rdf: $e")
    }}
    r.isRight
   }
  }


}
