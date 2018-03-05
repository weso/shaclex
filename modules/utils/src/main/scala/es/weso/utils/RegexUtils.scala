package es.weso.utils
import com.sun.org.apache.xerces.internal.impl.xpath.regex._

case class RegEx(pattern: String, maybeFlags: Option[String]) {
  val cleanPattern = cleanBackslashes(pattern)

  def cleanBackslashes(str: String): String = {
    str.replaceAllLiterally("\\\\", "\\")
  }
  def matches(str: String): Either[String, Boolean] = {
    try {
      val regex = new RegularExpression(cleanPattern, maybeFlags.getOrElse(""))
      Right(regex.matches(str))
    } catch {
      case e: Exception =>
        Left(s"Error: $e, matching $str with /$cleanPattern/${maybeFlags.getOrElse("")}")
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