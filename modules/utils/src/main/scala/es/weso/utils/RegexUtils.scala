package es.weso.utils
// import scala.util.matching.Regex
import com.sun.org.apache.xerces.internal.impl.xpath.regex.{ParseException, RegularExpression}

case class RegEx(pattern: String, maybeFlags: Option[String]) {
  def matches(str: String): Either[String,Boolean] = {
    try {
      val regex = new RegularExpression(pattern, maybeFlags.getOrElse(""))
      Right(regex.matches(str))
    } catch {
      case e: Exception => Left(s"Parse exception: $e when matching $str with /$pattern/${maybeFlags.getOrElse("")}")
    }
  }
}

object RegexUtils {

  // TODO: Use xerces implementation of XPath regex instead of Scala's builtin
  def makeRegex(pattern: String,
                flags: Option[String]): Either[String,RegEx] = {
    Right(RegEx(pattern,flags))
  }

  def regexMatch(pattern: RegEx, str: String): Either[String,Boolean] = {
    pattern.matches(str)
  }

}