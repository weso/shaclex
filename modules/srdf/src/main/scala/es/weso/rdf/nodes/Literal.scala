package es.weso.rdf.nodes

trait Literal extends RDFNode with Product with Serializable {
  def dataType: IRI

  def isLangLiteral: Boolean
  def hasLang(lang: Lang): Boolean
  def hasDatatype(iri: IRI): Boolean =
    dataType == iri
  override def getLexicalForm: String
}


