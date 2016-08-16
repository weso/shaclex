package tests

import org.scalatest._
import cats._, data._
import cats.implicits._


class MapsTest extends FunSpec with Matchers with TryValues with OptionValues {
  
  describe("Combining maps") {
    it("Should combine maps...") {
      type Evidence = List[String]
      type Error = NonEmptyList[String]
      case class TypingResult(r: Xor[Error,Evidence])
      
      type ShapeId = String
      type Node = String
      type Shapes = Map[ShapeId,TypingResult]
      type Typing = Map[Node, Shapes]
      val m1: Typing = Map(
          "x" -> Map("S" -> TypingResult(List("x+S").right), "T" -> TypingResult(List("x+T").right)),
          "y" -> Map("S" -> TypingResult(List("y+S").right)),
          "contradict" -> Map("S" -> TypingResult(List("c+S1","c+S2").right)),
          "wrong" -> Map("S" -> TypingResult(NonEmptyList("w!S1","w!S2").left))
          )
      val m2: Typing = Map(
          "x" -> Map("W" -> TypingResult(List("x+W").right), "S" -> TypingResult(List("x+S2").right)),
          "z" -> Map("S" -> TypingResult(NonEmptyList("z!S").left)),
          "contradict" -> Map("S" -> TypingResult(NonEmptyList("c!!!s").left)),
          "wrong" -> Map("S" -> TypingResult(NonEmptyList("w!S1","w!S3").left))
          )
          
      implicit def semigroupTypingResult = new Semigroup[TypingResult] {
        override def combine(t1: TypingResult, t2: TypingResult): TypingResult =
          TypingResult(t1.r |+| t2.r)
      }
      
      val comb = m1 |+| m2
      
      def showResult(r: TypingResult): String = {
        r.r.fold(error => "-" + error,evidence => "+" + evidence) 
      }
      
      val r: TypingResult = TypingResult(List("x - S", "x - S2").right)
      println(s"ShowResult = ${showResult(r)}") 
      def showShapes(s: Shapes) = {
        s.map{case (s,r) => {
          s"$s: ${showResult(r)}" }
        }.mkString(", ")
      }
      def showTyping(t: Typing) = {
        t.map{case (k,v) => {
          s"$k -> ${showShapes(v)}" }
        }.mkString("\n")
      }
      println(s"comb: \n${showTyping(comb)}")  
      comb should contain key "x"
      comb("x") should (contain key "S")
     // comb("x")("S") should contain ("x - S","x - S(2)")
    }
  }
}
