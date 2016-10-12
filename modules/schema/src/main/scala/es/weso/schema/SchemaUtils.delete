package es.weso.schema

import es.weso.shex.SchemaFormat

object SchemaUtils {

  val defaultSchemaFormat = SchemaFormat.default.name

  def getSchemaFormat(format: Option[String]): String = {
    format match {
      case Some(s) =>
        if (SchemaFormat.available(s)) s
        else // TODO: Check a better failure...
          throw new Exception("Unsupported schema format " + s)
      case None => defaultSchemaFormat
    }

  }

}
