package es.weso.utils.json
import io.circe._

object JsonTwirl {
  def json2htmlAttr(json: Json): String = {
    json.spaces2.replaceAllLiterally("'", "")
    // TODO: Check how to translate sinqle quotes
    // At this moment, single quotes are lost
    // The first version of this replaced single quotes by &quot; but the JSON parser complained
  }
}