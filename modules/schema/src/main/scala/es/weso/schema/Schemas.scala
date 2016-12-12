package es.weso.schema
import java.io.File

import util._
import es.weso.rdf.RDFReader
import es.weso.utils.FileUtils

import scala.io._
import scala.util.{Failure, Success, Try}

object Schemas {

type SchemaParser = (CharSequence,String,Option[String]) => Try[Schema]

lazy val shEx = ShExSchema.empty
lazy val shaclex = ShaclexSchema.empty
// lazy val shacl_tq = Shacl_TQ.empty

val availableSchemas: List[Schema] = List(shEx, shaclex) // shEx,shaclex) //,shacl_tq)
val defaultSchema : Schema = shaclex
val defaultSchemaName: String = defaultSchema.name
val defaultSchemaFormat: String = defaultSchema.defaultFormat

val availableSchemaNames: List[String] = availableSchemas.map(_.name)

val availableFormats: List[String] = {
  availableSchemas.map(_.formats).flatten.distinct
}

val availableTriggerModes: List[String] = {
    ValidationTrigger.triggerValues.map(_._1)
  }

val defaultTriggerMode: String =
  ValidationTrigger.default.name

def lookupSchema(schemaName: String): Try[Schema] = {
  if (schemaName == "") Success(defaultSchema)
  else {
  val found = availableSchemas.filter(_.name.compareToIgnoreCase(schemaName)==0)
  if (found.isEmpty)
    Failure(new Exception("Schema \"" + schemaName + "\" not found. Available schemas: " + availableSchemaNames.mkString(",")))
  else
    Success(found.head)
  }
}

def getSchemaParser(schemaName: String): Try[SchemaParser] = {
  lookupSchema(schemaName).map(_.fromString _)
}

val schemaNames: List[String] = availableSchemas.map(_.name)

def fromFile(
  file: File,
  format: String,
  schemaName: String,
  base: Option[String] = None): Try[Schema] = for {
  cs <- FileUtils.getContents(file)
  schema <- fromString(cs, format, schemaName, base)
} yield schema

def fromString(
  cs: CharSequence,
  format: String,
  schemaName: String,
  base: Option[String] = None): Try[Schema] = {
  lookupSchema(schemaName) match {
    case Success(schema) =>
      if (cs.length == 0) Success(schema.empty)
      else schema.fromString(cs,format,base)
    case Failure(e) => Failure(e)
  }
}

def fromRDF(rdf: RDFReader, schemaName: String): Try[Schema] = {
  for {
    defaultSchema <- lookupSchema(schemaName)
  ; schema <- defaultSchema.fromRDF(rdf)
  } yield schema
}

}
