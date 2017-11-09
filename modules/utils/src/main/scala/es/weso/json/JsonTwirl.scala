package es.weso.json
import io.circe._

object JsonTwirl {
  def json2htmlAttr(json: Json): String = {
    json.spaces2.replaceAllLiterally("'", "&quot;")
  }
}