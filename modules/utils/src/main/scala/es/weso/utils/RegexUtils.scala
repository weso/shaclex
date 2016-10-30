package es.weso.utils
import scala.util.matching.Regex

object RegexUtils {

  // TODO: Use xerces implementation of XPath regex instead of Scala's builtin
  def makeRegex(pattern: String,
                flags: Option[String]): Either[String,Regex] = {
    //TODO: Don't ignore flags
    //TODO: Check if there are errors in pattern
    Right(new Regex(pattern))
  }

  def regexMatch(pattern: Regex, str: String): Boolean = {
    pattern.findFirstIn(str).isDefined
  }

}