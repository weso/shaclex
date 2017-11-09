package es.weso.utils
import com.sun.org.apache.xerces.internal.impl.xpath.regex._

case class RegEx(pattern: String, maybeFlags: Option[String]) {
  val cleanPattern = cleanBackslashes(pattern)

  def cleanBackslashes(str: String): String = {
    str.replaceAllLiterally("\\\\", "\\")
  }
  def matches(str: String): Either[String, Boolean] = {
    println(s"Trying to match /$cleanPattern/${maybeFlags.getOrElse("")} with $str")
    try {
      val regex = new RegularExpression(cleanPattern, maybeFlags.getOrElse(""))
      val result = regex.matches(str)
      println(s"Result of match /$cleanPattern/${maybeFlags.getOrElse("")} with $str = $result")
      Right(result)
    } catch {
      case e: Exception => Left(s"Error: $e, matching $str with /$cleanPattern/${maybeFlags.getOrElse("")}")
    }
  }
}

/*object RegexUtils {

  def makeRegex(pattern: String,
                flags: Option[String]): Either[String,RegEx] = {
    Right(RegEx(pattern,flags))
  }

  def regexMatch(pattern: RegEx, str: String): Either[String,Boolean] = {
    pattern.matches(str)
  }

}*/ 