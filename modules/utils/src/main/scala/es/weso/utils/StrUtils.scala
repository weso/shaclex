package es.weso.utils
import org.apache.commons.text.StringEscapeUtils
import cats._
import cats.implicits._

object StrUtils {

  /**
    * Unscape unicode numbers
    * Given a string like: "p\u0031", return "p1"
    *
    * @param str input string
    * @return unscaped output string
    */
  def unescape(str: String): String = {

    // I will probably need to use other ways to unescape the string.
    // Some code I found in google: http://techidiocy.com/replace-unicode-characters-from-java-string/
    // https://udojava.com/2013/09/28/unescape-a-string-that-contains-standard-java-escape-sequences/

    // I will need to implement the following escape sequences: https://www.w3.org/TR/turtle/#sec-escapes
    // The following code doesn't handle uppercase \U

    // The following code implements the Turtle rules for unescaping strings https://www.w3.org/TR/turtle/#sec-escapes
    var index = 0
    val length = str.length
    val builder = new StringBuilder(length)
    while (index < str.length) {
      val (newChars: Array[Char],newIndex) = str(index) match {
        case c@'\\' => {
          val i = index + 1
          if (i >= str.length) (c,i) // should it be an error as last char = \
          else unescapeStringEscapeSequence(str, i).getOrElse(
            unescapeNumericSequence(str,i).getOrElse(
              unescapeReservedChar(str, i).getOrElse(
                (Array(c), i)
              )))
        }
        case c => (Array(c),index)
      }
      index = newIndex + 1
      builder.appendAll(newChars)
    }
    builder.toString

    //    StringEscapeUtils.unescapeJava(str)

  }

  type UnescapeResult = Either[String,(Array[Char],Int)]

  def unescapeStringEscapeSequence(str: String, index: Int): UnescapeResult = {
    str(index) match {
      case 't' => Right((Array('\u0009'), index))
      case 'b' => Right((Array('\u0008'), index))
      case 'n' => Right((Array('\u000A'), index))
      case 'r' => Right((Array('\u000D'), index))
      case 'f' => Right((Array('\u000C'), index))
      case '\"' => Right((Array('\u0022'), index))
      case '\'' => Right((Array('\''), index))
      case '\\' => Right((Array('\\'), index))
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
      case c => Right((Array(c),index))
    }

  }

  private def getHex(str: String, index: Int, num: Int): Int = {
    println(s"getHex: $str, index: $index, num: $num")
    val rs = (0 to num - 1).map(n => str(index + n)).mkString
    println(s"getHex. Result: $rs")
    Integer.parseInt(rs,16)
  }


  private def hexSeq2Char(cs: IndexedSeq[Int], len: Int): Char = {
    println(s"HexSeq: $cs")
    var x = 0
    (0 to len - 1).foreach(n => x << 4 + cs(n))
    x.toChar
  }

  private def unescapeReservedChar(str:String, index: Int): UnescapeResult = {
    str(index) match {
      case c if "~.-!$&'()*+,;=/?#@%_".contains(c) => Right((Array(c),index))
      case c => Left(s"Unsupported escape char $c")
    }
  }

}