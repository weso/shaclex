package es.weso.schema
import java.io.File

import util._
import es.weso.rdf.RDFReader
import es.weso.utils.FileUtils

object Schemas {

  type SchemaParser = (CharSequence, String, Option[String]) => Either[String, Schema]

  lazy val shEx: Schema = ShExSchema.empty
  lazy val shaclex : Schema = ShaclexSchema.empty
  // lazy val shacl_tq = Shacl_TQ.empty

  val availableSchemas: List[Schema] = List(shEx, shaclex) // shEx,shaclex) //,shacl_tq)
  val defaultSchema: Schema = shEx
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

  def lookupSchema(schemaName: String): Either[String, Schema] = {
    if (schemaName == "") Right(defaultSchema)
    else {
      val found = availableSchemas.filter(_.name.compareToIgnoreCase(schemaName) == 0)
      if (found.isEmpty)
        Left(s"Schema $schemaName not found. Available schemas: ${availableSchemaNames.mkString(",")}")
      else
        Right(found.head)
    }
  }

  def getSchemaParser(schemaName: String): Either[String, SchemaParser] = for {
    schema <- lookupSchema(schemaName)
  } yield schema.fromString _

  val schemaNames: List[String] = availableSchemas.map(_.name)

  def fromFile(
    file: File,
    format: String,
    schemaName: String,
    base: Option[String] = None): Either[String, Schema] = for {
    cs <- FileUtils.getContents(file)
    schema <- fromString(cs, format, schemaName, base)
  } yield schema

  def fromString(
    cs: CharSequence,
    format: String,
    schemaName: String,
    base: Option[String] = None): Either[String, Schema] = for {
    schema <- lookupSchema(schemaName)
    schemaParsed <- if (cs.length == 0) Right(schema.empty)
                    else schema.empty.fromString(cs, format, base)
  } yield schemaParsed

  def fromRDF(rdf: RDFReader, schemaName: String): Either[String, Schema] = {
    for {
      defaultSchema <- lookupSchema(schemaName)
      schema <- defaultSchema.fromRDF(rdf)
    } yield schema
  }

/*  def fromURI(uri: String, schemaName: String): Either[String, Schema] = {
    for {
      defaultSchema <- lookupSchema(schemaName)
      schema <- defaultSchema.fromURI(rdf)
    } yield schema
  } */
}
