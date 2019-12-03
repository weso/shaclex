package es.weso.schema
import java.io.File

import util._
import cats.effect._
import cats.data.EitherT
import es.weso.rdf.RDFReader
import es.weso.utils.FileUtils._

object Schemas {

  type SchemaParser = (CharSequence, String, Option[String]) => EitherT[IO, String, Schema]

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

  def getSchemaParser(schemaName: String): EitherT[IO, String, SchemaParser] = for {
    schema <- EitherT.fromEither[IO](lookupSchema(schemaName))
    parser = schema.fromString _
  } yield parser

  val schemaNames: List[String] = availableSchemas.map(_.name)

  def fromFile(
    file: File,
    format: String,
    schemaName: String,
    base: Option[String] = None): EitherT[IO, String, Schema] = for {
    cs <- getContents(file)
    schema <- fromString(cs, format, schemaName, base)
  } yield schema

  def fromString(
    cs: CharSequence,
    format: String,
    schemaName: String,
    base: Option[String] = None): EitherT[IO, String, Schema] = for {
    schema <- EitherT.fromEither[IO](lookupSchema(schemaName))
    schemaParsed <- if (cs.length == 0) EitherT.pure[IO,String](schema.empty)
                    else schema.empty.fromString(cs, format, base)
  } yield schemaParsed

  def fromRDF(rdf: RDFReader, schemaName: String): EitherT[IO, String, Schema] = {
    for {
      defaultSchema <- EitherT.fromEither[IO](lookupSchema(schemaName))
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
