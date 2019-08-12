package es.weso.utils
import com.sun.org.apache.xerces.internal.impl.xpath.regex._

case class RegEx(pattern: String, maybeFlags: Option[String]) {
  val cleanPattern = cleanBackslashes(pattern)

  def cleanBackslashes(str: String): String = {
    val s = str.replaceAllLiterally("\\\\d", "\\d")
    println(s"Clean pattern str: $str=$s")
    s
  }
  def matches(str: String): Either[String, Boolean] = {
    println(s"Pattern: $pattern\ncleanPattern: $cleanPattern")
    println(s"str: $str")
    // println(s"re: $cleanPattern: chars: ${cleanPattern.map(c => c.toInt).mkString(",")}")
    try {
      val regex = new RegularExpression(cleanPattern, maybeFlags.getOrElse(""))
      println(s"Regex: ${regex} matches: ${regex.matches(str)}")
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