package es.weso.utils

object StrUtils {

  /**
    * Converter takes an index i into a string and checks how many charts it can replace
    * If it can't replace, it returns None (no conversion)
    * If it replaces, it returns the characters that are replaced and the next index in the string
    * Rationale: Some conversions may require some lookahead, in which case, the index will be i + characters read
    */
  type Converter = (String, Int) => Option[CharConversion]
  type CharConversion = (Array[Char],Int)

  /**
    * Unescape unicode numbers
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
      // unescapeStringEscapeSequence,
      unescapeNumericSequence,
      unescapeBackSlash
      // unescapeReservedPatternChar,
      // unescapeReservedChar
    )
  )

  private def cnvChar(c: Char, i: Int): Option[CharConversion] =
    Some((Array(c), i))

  private def unescapeBackSlash: Converter = (str,i) =>
    if (str(i) == '\\') {
      str(i + 1) match {
        case '\\' => Some((Array('\\','\\'), i + 1))
        case _ => None
      }
    } else None

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
        // println(s"unescape pattern: $c")
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

  private def noConverter(str: String, i: Int): CharConversion =
   (Array(str(i)),i)

  /**
    * escapeDot: Escapes strings to be represented as labels in Dot
    * It follows dot conventions: https://graphviz.gitlab.io/_pages/doc/info/lang.html
    * Extra characters are escaped using their Unicode representation
    * @param str
    * @return
    */
  def escapeDot(str: String): String = cnvLoop(str, List(dotConverter))

  private def dotConverter: Converter = (str,i) => str(i) match {
    case c if c.isLetterOrDigit => None
    case c if c.toInt > 200 && c.toInt < 377 => None
    case c => {
      Some((s"&#${c.toInt};".toCharArray, i))
    }
  }

}