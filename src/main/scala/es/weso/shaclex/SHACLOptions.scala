package es.weso.shaclex

import es.weso.rdf.nodes._

case class SHACLsOptions(
   verbose: Boolean,
   dataFormat: String,
   schemaFormat: String,
   base: Option[IRI]
)

object SHACLOptions {
   def defaultOptions(): SHACLsOptions = 
      new SHACLsOptions(
          verbose = false, 
          dataFormat = "TURTLE",
          schemaFormat = "TURTLE",
          base = None
          )
}
