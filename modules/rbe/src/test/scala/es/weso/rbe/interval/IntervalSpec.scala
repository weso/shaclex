package es.weso.rbe.interval

import org.scalacheck._
import es.weso.collection._
import es.weso.rbe._
import IntOrUnbounded.int2LimitInt

trait GenRbe extends GenBag {

  // Generates a controlled set of cardinalities
  val genCard: Gen[(Int, Int)] = Gen.oneOf(
    (0, 0), (0, 1), (1, 1), (0, 2), (1, 2), (2, 2), (3, 2), (20, 20), (1, 20), (2, 20), (3, 20))

  def genEmpty: Gen[Rbe[Char]] =
    Gen.const(Empty)

  def genSymbol: Gen[Rbe[Char]] = for {
    (m, n) <- genCard
    a <- letter
  } yield Symbol(a, m, n)

  def genAnd(level: Int): Gen[Rbe[Char]] = for {
    v1 <- genRbe(level)
    v2 <- genRbe(level)
  } yield And(v1, v2)

  def genOr(level: Int): Gen[Rbe[Char]] = for {
    v1 <- genRbe(level)
    v2 <- genRbe(level)
  } yield Or(v1, v2)

  def genPlus(level: Int): Gen[Rbe[Char]] = for {
    v <- genRbe(level)
  } yield Plus(v)

  def genStar(level: Int): Gen[Rbe[Char]] = for {
    v <- genRbe(level)
  } yield Star(v)

  def genRbe(level: Int): Gen[Rbe[Char]] =
    if (level >= 5) Gen.oneOf(genEmpty, genSymbol)
    else {
      val newLevel = level + 1
      Gen.oneOf(
        genEmpty,
        genSymbol,
        genAnd(newLevel),
        genOr(newLevel),
        genPlus(newLevel),
        genStar(newLevel))
    }

  def rbe: Gen[Rbe[Char]] = genRbe(0)

}

/*class IntervalSpec
 extends PropSpec
 with GenRbe
 with Matchers
 with GeneratorDrivenPropertyChecks {


  def open : Gen[Boolean] = Gen.oneOf(true,false)

  def condition(c:Char) = c >= 'a'

  val smallInteger = Gen.choose(0,100)

  val propSmallInteger = Prop.forAll(smallInteger)(n => n >= 0 && n <= 100)

  property("letter in range") {
    forAll(letter)(l => condition(l))
  }

  property("intervals I(E)") {
    forAll(rbe,bagOfCharFromContainer)((e,bag) => {
      IntervalChecker.interval(e,bag).m >= 0
    })
  }

}
*/
//object IntervalSpec extends IntervalSpec