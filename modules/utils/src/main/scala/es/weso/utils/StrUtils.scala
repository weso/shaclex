package es.weso.utils

object StrUtils {

  /**
    * Unscape unicode numbers
    * Given a string like: "p\u0031", return "p1"
    * The code implements the Turtle rules: https://www.w3.org/TR/turtle/#sec-escapes
    *
    * @param str input string
    * @return unscaped output string
    */
  def unescape(str: String): String = {
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

  type UnescapeResult = Either[String,(Array[Char],Int)]

  private def rightChar(c: Char, i: Int): UnescapeResult =
    Right((Array(c), i))

  private def unescapeStringEscapeSequence(str: String, i: Int): UnescapeResult = {
    str(i) match {
      case 't' => rightChar('\u0009', i)
      case 'b' => rightChar('\u0008', i)
      case 'n' => rightChar('\u000A', i)
      case 'r' => rightChar('\u000D', i)
      case 'f' => rightChar('\u000C', i)
      case '\"' => rightChar('\u0022', i)
      case '\'' => rightChar('\'', i)
      case '\\' => rightChar('\\', i)
      case c => Left(s"Unrecognized escape character \\$c")
    }
  }

  private def unescapeNumericSequence(str: String, index: Int): UnescapeResult = {
    str(index) match {
      case 'u' => {
        val hexValue = getHex(str,index + 1,4)
        Right((Character.toChars(hexValue), index + 4))
      }
      case 'U'  => {
        val hexValue = getHex(str,index + 1,8)
        Right((Character.toChars(hexValue),index + 8))
      }
      case c => Left(s"Non numeric sequence")
    }
  }

  private def getHex(str: String, index: Int, num: Int): Int = {
    val rs = (0 to num - 1).map(n => str(index + n)).mkString
    Integer.parseInt(rs,16)
  }

  private def unescapeReservedPatternChar(str:String, index: Int): UnescapeResult = {
    str(index) match {
      case c if "^$[]".contains(c) => {
        println(s"unescape pattern: $c")
        Right((Array('\\',c),index))
      }
      case c => Left(s"Unsupported escape char $c")
    }
  }

  private def unescapeReservedChar(str:String, index: Int): UnescapeResult = {
    str(index) match {
      case c if "~.-!$&'()*+,;=/?#@%_".contains(c) => Right((Array(c),index))
      case c => Left(s"Unsupported escape char $c")
    }
  }

  /**
    * Escape a string
    * Example: "Hi\n\t" -> "Hi\\n\\t"
    * @param str
    * @return
    */
  def escape(str: String): String = {
    var i = 0
    val length = str.length
    val builder = new StringBuilder(length)
    while (i < str.length) {
      val (nextChars,newIndex) = str(i) match {
        case '\t' => escapeChar('t',i)
        case '\b' => escapeChar('b',i)
        case '\n' => escapeChar('n',i)
        case '\r' => escapeChar('r',i)
        case '\f' => escapeChar('f',i)
        case '\'' => escapeChar('\'',i)
        case '\"' => escapeChar('\"',i)
        case '\\' => {
          val newIndex = i + 1
          str(newIndex) match {
            case '^' => (Array('\\','^'),newIndex)
            case '$' => (Array('\\','$'),newIndex)
            case _ => (Array('\\','\\'),i)
          }
        }
//        case '/' => escapeChar('/')
        case '-' => escapeChar('-',i)
//        case '^' => escapeChar('^')
//        case '$' => escapeChar('$')

        // case c if "~.-!$&'()*+,;=/?#@%_" contains c => escapeChar(c)
        case c => (Array(c),i)
      }
      i = newIndex + 1
      builder.appendAll(nextChars)
    }
    builder.mkString
  }

  private def escapeChar(c: Char, i: Int) = (Array('\\',c), i)
}