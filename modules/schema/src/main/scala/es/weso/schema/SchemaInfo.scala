package es.weso.schema

case class SchemaInfo(schemaName: String,
                      schemaEngine: String,
                      isWellFormed: Boolean,
                      errors: List[String]
                     )