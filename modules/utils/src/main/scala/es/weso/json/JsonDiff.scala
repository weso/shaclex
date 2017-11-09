package es.weso.json
import io.circe._
import gnieh.diffson.circe._

/*
 * Obtains a diff between 2 Jsons
 */
object JsonCompare {

  def jsonDiff(json1: Json, json2: Json): JsonPatch = {
    JsonDiff.diff(json1, json2, false)
  }

  /**
   * Obtains a diff between 2 Jsons
   *
   */
  def diff(json1: Json, json2: Json): String = {
    diff(json1, json2, 0)
  }

  private val OK = ""

  private def diff(json1: Json, json2: Json, indent: Int): String = {
    json1.fold(
      checkNull(indent)(json2),
      checkBool(indent)(json2),
      checkNumber(indent)(json2),
      checkString(indent)(json2),
      checkArray(indent)(json2),
      checkObject(indent)(json2))
  }

  private def checkNull(indent: Int)(json: Json): String = {
    if (json.isNull)
      OK
    else
      mkIndent(indent, s"json $json should be null")
  }

  private def checkBool(indent: Int)(json: Json)(v: Boolean): String = {
    if (json.isBoolean) {
      if (json.asBoolean.get == v) {
        OK
      } else
        mkIndent(indent, s"json $json should be the boolean $v")
    } else mkIndent(indent, s"json $json should be bool $v")
  }

  private def checkNumber(indent: Int)(json: Json)(v: JsonNumber): String = {
    if (json.isNumber) {
      if (json.asNumber.get == v) {
        OK
      } else
        mkIndent(indent, s"json $json should be number $v")
    } else
      mkIndent(indent, s"json $json should be a numer to compare with $v")
  }

  private def checkString(indent: Int)(json: Json)(v: String): String = {
    if (json.isString) {
      val str = json.asString.get
      if (str == v) {
        OK
      } else
        mkIndent(indent, s"$str should be $v")
    } else
      mkIndent(
        indent,
        s"json $json should be a string in order to compare with $v")
  }

  private def checkArray(indent: Int)(json: Json)(v: Vector[Json]): String = {
    if (json.isArray) {
      val array: Vector[Json] = json.asArray.get
      val d = diffArrays(indent)(array, v)
      if (d == OK) {
        OK
      } else {
        s"Arrays different: $d"
      }
    } else mkIndent(indent, s"json $json should be array $v")
  }

  private def checkObject(indent: Int)(json: Json)(v: JsonObject): String = {
    if (json.isObject) {
      val obj = json.asObject.get
      val d = diffObjects(indent + 1)(obj, v)
      if (d == OK) {
        OK
      } else {
        mkIndent(indent, s"Objects different...$d")
      }
    } else
      mkIndent(indent, s"json $json should be object to be able to compare with $v")
  }

  private def mkIndent(indent: Int, msg: String): String = {
    "\n" + (" " * indent) + msg
  }

  private def diffObjects(indent: Int)(o1: JsonObject, o2: JsonObject): String = {
    val zero = ""
    def cont: ((String, Json), String) => String = { (pair, rest) =>
      val (field, value) = pair
      o1(field) match {
        case None => mkIndent(indent, s"obj1 $o1 doesn't contain field $field with value $value" + rest)
        case Some(v) => diff(value, v, indent) + rest
      }
    }
    val fieldsDifferent = diffFields(o1, o2)
    if (fieldsDifferent.isEmpty)
      o2.toMap.foldRight(zero)(cont)
    else {
      mkIndent(indent, s"Fields different: ${fieldsDifferent.get}")
    }
  }

  private def diffFields(
    o1: JsonObject, o2: JsonObject): Option[(Set[String], Set[String])] = {
    val fields1minus2 = o1.keys.toSet -- o2.keys.toSet
    val fields2minus1 = o2.keys.toSet -- o1.keys.toSet
    if (fields1minus2.isEmpty && fields2minus1.isEmpty)
      None
    else
      Some((fields1minus2, fields2minus1))
  }

  private def diffArrays(indent: Int)(
    o1: Vector[Json], o2: Vector[Json]): String = {
    if (o1.length == o2.length) {
      val zero = ""
      def cont: (((Json, Json), Int), String) => String = { (t, rest) =>
        val ((v1, v2), n) = t
        val d = diff(v1, v2, indent + 1)
        if (d == "")
          rest
        else
          mkIndent(
            indent,
            s"Array diff at index $n: $d$rest")
      }
      (o1.toList zip o2.toList zip (1 to o1.length)).foldRight(zero)(cont)
    } else {
      mkIndent(
        indent,
        s"Arrays have different lengths: ${o1.length}!=${o2.length}")
    }
  }

}
