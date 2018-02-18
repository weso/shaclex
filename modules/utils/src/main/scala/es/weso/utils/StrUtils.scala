package es.weso.utils
import cats._
import cats.implicits._

object StrUtils {

  /**
    * Unscape unicode numbers
    * Given a string like: "p\u0031", return "p1"
    * The code implements the Turtle rules: https://www.w3.org/TR/turtle/#sec-escapes
    *
    * @param str input string
    * @return unscaped output string
    */

  def unescapeStringLiteral(str: String): String = cnvLoop(str,
    List(
      unescapeStringEscapeSequence,
      unescapeNumericSequence,
      unescapeReservedChar)
  )

  def unescapeIRI(str: String): String = unescapeStringLiteral(str)

  def unescapeCode(str: String): String = cnvLoop(str,
    List(
      unescapeStringEscapeSequence,
      unescapeNumericSequence,
      unescapeReservedPatternChar,
      unescapeReservedChar)
  )

  def unescapePattern(str: String): String = cnvLoop(str,
    List(
      unescapeStringEscapeSequence,
      unescapeNumericSequence,
      unescapeReservedPatternChar,
      unescapeReservedChar)
  )

  /*  def unescape(str: String): String = {
    println(s"Unescaping $str")
    var index = 0
    val length = str.length
    val builder = new StringBuilder(length)
    while (index < str.length) {
      val (newChars: Array[Char],newIndex) = str(index) match {
        case c@'\\' => {
          val i = index + 1
          if (i >= str.length) (c,i) // should it be an error as last char = \ ?
          else {
            println(s"Found escape at ${str(i)}")
            unescapeStringEscapeSequence(str, i).getOrElse(
              unescapeNumericSequence(str, i).getOrElse(
                unescapeReservedPatternChar(str, i).getOrElse(
                  unescapeReservedChar(str, i).getOrElse(
                  (Array(c), i)
                ))))
          }
        }
        case c => (Array(c),index)
      }
      index = newIndex + 1
      builder.appendAll(newChars)
    }

    val s = builder.mkString
    println(s"Result of unescape: $str = $s")
    s

    // Other alternatives that we found:
    // Use Apache commons: import org.apache.commons.text.StringEscapeUtils
    // StringEscapeUtils.unescapeJava(str)
    // But it doesn't handle some parts

    // Some code I found in google: http://techidiocy.com/replace-unicode-characters-from-java-string/
    // https://udojava.com/2013/09/28/unescape-a-string-that-contains-standard-java-escape-sequences/
    // Jena also implements the same rules, but it seemed to fail with \- escapes
    // Jena's code: https://github.com/apache/jena/blob/master/jena-base/src/main/java/org/apache/jena/atlas/lib/StrUtils.java
  }
*/

  private def cnvChar(c: Char, i: Int): Option[CharConversion] =
    Some((Array(c), i))

  private def unescapeStringEscapeSequence: Converter = (str,i) =>
   if (str(i) == '\\') {
    str(i + 1) match {
      case 't' => cnvChar('\u0009', i + 1)
      case 'b' => cnvChar('\u0008', i + 1)
      case 'n' => cnvChar('\u000A', i + 1)
      case 'r' => cnvChar('\u000D', i + 1)
      case 'f' => cnvChar('\u000C', i + 1)
      case '\"' => cnvChar('\u0022', i + 1)
      case '\'' => cnvChar('\'', i + 1)
      case '\\' => cnvChar('\\', i + 1)
      case _ => None
    }
  } else None

  private def unescapeNumericSequence: Converter = (str,i) =>
    if (str(i) == '\\') {
     str(i + 1) match {
      case 'u' => {
        val hexValue = getHex(str,i + 2, 4)
        Some((Character.toChars(hexValue),i + 5))
      }
      case 'U'  => {
        val hexValue = getHex(str,i + 2, 8)
        Some((Character.toChars(hexValue),i + 9))
      }
      case _ => None
    }
  } else None

  private def getHex(str: String, index: Int, num: Int): Int = {
    val rs = (0 to num - 1).map(n => str(index + n)).mkString
    Integer.parseInt(rs,16)
  }

  private def unescapeReservedPatternChar: Converter = (str,i) =>
   if (str(i) == '\\') {
    str(i + 1) match {
      case c if "^$[]".contains(c) => {
        println(s"unescape pattern: $c")
        Some((Array('\\',c),i + 1))
      }
      case c => None
    }
  } else None

  private def unescapeReservedChar: Converter = (str,i) =>
   if (str(i) == '\\') {
    str(i + 1) match {
      case c if "~.-!$&'()*+,;=/?#@%_".contains(c) => cnvChar(c,i + 1)
      case c => None
    }
   } else None

  /**
    * Escape a string
    * Example: "Hi\n\t" -> "Hi\\n\\t"
    * @param str
    * @return
    */

  def escapeStringLiteral(str: String): String = cnvLoop(str, List(cnvCtrl))

  def escapePattern(str: String): String = cnvLoop(str,List())

  type Converter = (String, Int) => Option[CharConversion]
  type CharConversion = (Array[Char],Int)

  def cnvLoop(str: String, converters: List[Converter]): String = {
    var i = 0
    val length = str.length
    val builder = new StringBuilder(length)
    while (i < str.length) {
      val (nextChars,newIndex) = {
        val zero: CharConversion = noConverter(str,i)
        def next(f: Converter, r: CharConversion): CharConversion = {
          f(str,i).getOrElse(r)
        }
        converters.foldRight(zero)(next)
      }
      i = newIndex + 1
      builder.appendAll(nextChars)
    }
    builder.mkString
  }

/*  def escape(str: String): String = {
    var i = 0
    val length = str.length
    val builder = new StringBuilder(length)
    while (i < str.length) {
      val (nextChars,newIndex) = cnvCtrl(str,i).getOrElse(noConverter(str,i))
      i = newIndex + 1
      builder.appendAll(nextChars)
    }
    builder.mkString
  } */

  private def escapeChar(c: Char, i: Int) = Some((Array('\\',c), i))

  private def cnvCtrl: Converter = (str,i) =>
   str(i) match {
    case '\t' => escapeChar('t', i)
    case '\b' => escapeChar('b', i)
    case '\n' => escapeChar('n', i)
    case '\r' => escapeChar('r', i)
    case '\f' => escapeChar('f', i)
    case '\'' => escapeChar('\'', i)
    case '\"' => escapeChar('\"', i)
    case _ => None
  }

  private def cnvBackslashPattern: Converter = (str,i) =>
   str(i) match {
    case '\\' => {
      val newIndex = i + 1
      str(newIndex) match {
        case '^' => Some((Array('^'), newIndex))
        case '$' => Some((Array('$'), newIndex))
        case _ => Some((Array('\\'), i))
      }
    }
    case _ => None
  }


  private def noConverter(str: String, i: Int): CharConversion =
   (Array(str(i)),i)

}