package es.weso.schemaInfer
import es.weso.rdf.nodes.IRI

case class InferredNodeValue(constraint: InferredNodeConstraint,
                             number: Int) {
  /**
    * Returns the list of IRIs employed by the node constraint
    * @return
    */
  def getIRI: Option[IRI] = constraint.getIRI

}

