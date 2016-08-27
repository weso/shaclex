package es.weso.shacl

import es.weso.rdf.nodes.IRI
import es.weso.rdf.PREFIXES._

object SHACLPrefixes {

  def add(iri: IRI, str: String): IRI = {
    iri + str
  }

  lazy val sh = IRI("http://www.w3.org/ns/shacl#")
  
  lazy val sh_IRI = sh + "IRI"
  lazy val sh_Literal = sh + "Literal"
  lazy val sh_BlankNode = sh + "BNode"
  lazy val sh_IRIOrLiteral = sh + "IRIOrLiteral"
  lazy val sh_BlankNodeOrIRI = sh +  "BlankNodeOrIRI"
  lazy val sh_BlankNodeOrLiteral = sh +  "BlankNodeOrLiteral"
  
  lazy val sh_Shape = sh + "Shape"
  lazy val sh_property = sh + "property"
  lazy val sh_predicate = sh + "predicate"
  lazy val sh_PropertyConstraint = sh + "PropertyConstraint"

  lazy val sh_class = sh + "class"
  lazy val sh_datatype = sh + "datatype"
  lazy val sh_nodeKind = sh + "nodeKind"
  lazy val sh_minCount = sh + "minCount"
  lazy val sh_maxCount = sh + "maxCount"
  lazy val sh_minInclusive = sh + "minInclusive"
  lazy val sh_minExclusive = sh + "minExclusive"
  lazy val sh_maxInclusive = sh + "maxInclusive"
  lazy val sh_maxExclusive = sh + "maxExclusive"
  lazy val sh_minLength = sh + "minLength"
  lazy val sh_maxLength = sh + "maxLength"
  lazy val sh_pattern = sh + "pattern"
  lazy val sh_flags = sh + "flags"
  lazy val sh_stem = sh + "stem"
  lazy val sh_uniqueLang = sh + "uniqueLang"
  lazy val sh_equals = sh + "equals"
  lazy val sh_disjoint = sh + "disjoint"
  lazy val sh_lessThan = sh + "lessThan"
  lazy val sh_lessThanOrEquals = sh + "lessThanOrEquals"
  lazy val sh_not = sh + "not"
  lazy val sh_or = sh + "or"
  lazy val sh_and = sh + "and"
  lazy val sh_shape = sh + "shape"
  lazy val sh_qualifiedValueShape = sh + "qualifiedValueShape"
  lazy val sh_qualifiedMinCount = sh + "qualifiedMinCount"
  lazy val sh_qualifiedMaxCount = sh + "qualifiedMaxCount"
  lazy val sh_closed = sh + "closed"
  lazy val sh_ignoredProperties = sh + "ignoredProperties"
  lazy val sh_hasValue = sh + "hasValue"
  lazy val sh_in = sh + "in"
  lazy val sh_name = sh + "name"
  lazy val sh_description = sh + "description"
  
  lazy val sh_Schema = sh + "Schema"
  lazy val sh_targetNode = sh + "targetNode"
  lazy val sh_targetClass = sh + "targetClass"
  lazy val sh_targetSubjectsOf = sh + "targetSubjectsOf"
  lazy val sh_targetObjectsOf = sh + "targetObjectsOf"
  
  lazy val sh_text = sh + "text"
  

}