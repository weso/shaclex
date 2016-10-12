package es.weso.schema
import util._
import es.weso.rdf.RDFReader

object Schemas {

type SchemaParser = (CharSequence,String,Option[String]) => Try[Schema]

// lazy val shEx = ShEx3.empty
lazy val shaclex = Shaclex.empty
// lazy val shacl_tq = Shacl_TQ.empty

val availableSchemas: List[Schema] = List(shaclex) // shEx,shaclex) //,shacl_tq)
val defaultSchema : Schema = shaclex
val defaultSchemaName: String = defaultSchema.name
val defaultSchemaFormat: String = defaultSchema.defaultFormat

val availableSchemaNames: List[String] = availableSchemas.map(_.name)

val availableFormats: List[String] = {
  availableSchemas.map(_.formats).flatten.distinct
}

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

def fromString(cs: CharSequence, format: String, schemaName: String, base: Option[String] = None): Try[Schema] = {
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
