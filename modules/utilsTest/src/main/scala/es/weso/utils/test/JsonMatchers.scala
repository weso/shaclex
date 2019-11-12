package es.weso.utils.test

import org.scalatest.matchers.{MatchResult, Matcher}
import io.circe._
import io.circe.parser._
import diffson._
import diffson.lcs._
import diffson.circe._
import diffson.jsonpatch.lcsdiff._

/**
* JSONMatchers: some parts of this code have been taken from
 * https://github.com/stephennancekivell/scalatest-json/blob/master/circe/src/main/scala/com/stephenn/scalatest/circe/JsonMatchers.scala
 */

trait JsonMatchers {

  class JsonMatcher(expected: Json) extends Matcher[Json] {
    def apply(left: Json) = {
      matchJsonResult(left.spaces2,expected.spaces2,left,expected)
    }
  }

  /**
  * Allows to compare a Json with another Json
   * @param expected
   * @return
   */
  def matchJsonJson(expected: Json) = new JsonMatcher(expected)

  /**
   * Allows to compare a string representing a Json with another string representing a Json
   */
  def matchJson(right: String): Matcher[String] = {
    Matcher[String] { left =>
      (parse(left), parse(right)) match {
        case (Right(leftJson), Right(rightJson)) =>
          matchJsonResult(left, right, leftJson, rightJson)
        case _ =>
          cantParseResult(left, right)
      }
    }
  }

  /**
   * Allows to compare a Json with a string representing a Json
   */
  def matchJsonString(right: String): Matcher[Json] = {
    Matcher[Json] { left =>
      parse(right) match {
        case Right(rightJson) =>
          matchJsonResult(left.spaces2, right, left, rightJson)
        case _ =>
          cantParseResult(left.spaces2, right.trim)
      }
    }
  }

  private def matchJsonResult(left: String,
                              right: String,
                              leftJson: Json,
                              rightJson: Json) =
    MatchResult(
      matches = leftJson == rightJson,
      rawFailureMessage =
        "Json did not match {0} did not match {1}\n\nJson Diff:\n{2}",
      rawNegatedFailureMessage =
        "Json should not have matched {0} matched {1}\n\nJson Diff:\n{2}",
      args = Array((left.trim, right.trim, diffMessage(leftJson, rightJson))).toIndexedSeq
    )

  private def cantParseResult(left: String, right: String) = MatchResult(
    matches = false,
    rawFailureMessage = "Could not parse json {0} did not equal {1}",
    rawNegatedFailureMessage = "Json should not have matched {0} {1}",
    args = Array((left.trim, right.trim)).toIndexedSeq
  )

  private def diffMessage(left: Json, right: Json): String = {
    implicit val lcs = new Patience[Json]
    diff(left, right).toString()
  }
}

/**
* This line allows to import JsonMatchers without requiring to extend the trait
 */
object JsonMatchers extends JsonMatchers