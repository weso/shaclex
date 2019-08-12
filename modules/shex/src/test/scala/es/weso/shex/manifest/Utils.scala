package es.weso.shex.manifest
import java.net.URI

import es.weso.rdf.nodes.IRI

object Utils {

  /**
    * Example:
    * {{{ if iri = IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/1dot.shex")
    * baseGlobal = https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest
    * baseLocal = file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas
    * returns: file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas/1dot.shex
    * }}}
    * @param iri
    * @param baseGlobal
    * @param baseLocal
    * @return
    */
  def mkLocal(iri: IRI, baseGlobal: URI, baseLocal: URI): URI = {
    val parentGlobal = baseGlobal.resolve("..").toString
    val parentLocal = baseLocal.resolve("..").toString
    val resolved = new java.net.URI(iri.uri.toString.replaceFirst(parentGlobal,parentLocal))
    resolved
  }

  val negativeSyntaxBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeSyntax/manifest")
  val negativeStructureBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeStructure/manifest")
  val schemasBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest")
  val validationBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/validation/manifest")

}