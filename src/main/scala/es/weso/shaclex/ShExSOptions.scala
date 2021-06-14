package es.weso.shaclex

import es.weso.rdf.nodes._

case class ShExsOptions(
   verbose: Boolean,
   dataFormat: String,
   schemaFormat: String,
   shapemapFormat: String,
   base: Option[IRI]
)

object ShExsOptions {
   def defaultOptions(): ShExsOptions = 
      new ShExsOptions(
          verbose = false, 
          dataFormat = "TURTLE",
          schemaFormat = "ShExC",
          shapemapFormat = "Compact",
          base = None
          )
}
