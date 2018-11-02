package es.weso.shex.validator
import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest._

import scala.util.matching.Regex

class RegexTest extends FunSpec with Matchers with EitherValues {
  val rdf = RDFAsJenaModel.empty

  describe(s"Regex test") {

    checkMatch("a", """\u0061""".r)
    checkMatch(List(92,97).map(_.toChar).mkString, """\\a""".r)
    checkMatch(List(55349, 56504).map(_.toChar).mkString, """𝒸""".r)
    // 92 = \ 97 = a  110 = n  94 = ^, 36 = $, 47 = /  9 = ^I
    checkMatchLs(
      //       / ^t    ^n     \r  -     \  a
      List(   47,9,    10,    13,45,   92,97,55349,56504),
      //    ^  / ^I \   n  \   r  -  \  \  a              $
      List(94,47,9,92,110,92,114,45,92,92,97,55349,56504,36)
    )

    def checkMatchLs(ls: List[Int], rs: List[Int]): Unit = {
      val str = ls.map(_.toChar).mkString
      val re = rs.map(_.toChar).mkString.r
      checkMatch(str,re)
    }

      def checkMatch(str: String, re: Regex): Unit = {
        it(s"$str should match $re") {
          re.findFirstMatchIn(str) match {
            case Some(_) => info(s"$str matches $re")
            case None    => fail(s"$str does not match $re")
          }
        }
      }
    }
}
