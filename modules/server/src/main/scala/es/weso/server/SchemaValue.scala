package es.weso.server

case class SchemaValue(
                        schema: Option[String],
                        schemaURL: Option[String],
                        currentSchemaFormat: String,
                        availableSchemaFormats: List[String],
                        currentSchemaEngine: String,
                        availableSchemaEngines: List[String],
                        activeSchemaTab: String
                      )