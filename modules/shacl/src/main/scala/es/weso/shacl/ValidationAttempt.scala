package es.weso.shacl

import util._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._

trait ValidationAttempt {
  def isValid: Boolean

  def show(
    verbose: Boolean = false,
    cut: Int = 1,
    prefixMap: PrefixMap = PrefixMap.empty): String
}

case class ScopeNodeAttempt(
  node: IRI,
  shape: Shape,
  schema: Schema,
  result: Seq[ViolationError]) extends ValidationAttempt {

  override def isValid: Boolean = {
    result.isEmpty
  }

  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    val sb: StringBuilder = new StringBuilder
    if (isValid) {
      sb ++= s"$node has shape $shape"
      /*      if (verbose) {
       sb ++= result.show(cut)(prefixMap)
      } */
    } else {
      sb ++= s"Failure: Node $node doesn't have shape $shape"
      /*      if (verbose) {
        sb ++= s"Result: ${result.show(cut)(prefixMap)}"
      } */
    }
    sb.toString
  }
}

case class ScopeClassAttempt(
  cls: RDFNode,
  shape: Shape,
  schema: Schema,
  result: Seq[ViolationError]) extends ValidationAttempt {

  override def isValid = false

  override def show(verbose: Boolean, cut: Int = 1, prefixMap: PrefixMap = PrefixMap.empty): String = {
    s"ScopeClassAttempt: $cls with $shape "
  }
}

object ValidationAttempt {

  def showAttempts(
    attempts: Seq[ValidationAttempt],
    verbose: Boolean,
    cut: Int,
    pm: PrefixMap): String = {
    if (attempts.isEmpty) {
      "No declared validation detected (sh:scopeNode/sh:scopeClass)"
    } else {
      val sb = new StringBuilder
      for (attempt <- attempts) {
        sb ++= attempt.show(verbose, cut, pm) + "\n"
      }
      sb.toString
    }
  }
}

