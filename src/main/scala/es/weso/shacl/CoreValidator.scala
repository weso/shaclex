package es.weso.shacl

import es.weso.rdf._
import es.weso.rdf.nodes._

/**
 * This validator will be implemented directly in Scala
 */
trait CoreValidator {
  
  def validate(graph: RDFReader, schema: Schema): Seq[ViolationError] = {
    Seq()
  }
  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def scopeNodes(graph: RDFReader, schema: Schema): Seq[(RDFNode,Shape)] = {
    Seq()
  }
}