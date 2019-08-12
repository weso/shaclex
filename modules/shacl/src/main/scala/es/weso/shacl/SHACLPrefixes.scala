package es.weso.shacl

import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES._
import es.weso.rdf._

object SHACLPrefixes {

//  lazy val sh = IRI("http://www.w3.org/ns/shacl#")

  lazy val `sh:BlankNode`: IRI = sh + "BlankNode"
  lazy val `sh:BlankNodeOrIRI`: IRI = sh + "BlankNodeOrIRI"
  lazy val `sh:BlankNodeOrLiteral`: IRI = sh + "BlankNodeOrLiteral"
  lazy val `sh:Info`: IRI = sh + "Info"
  lazy val `sh:IRI`: IRI = sh + "IRI"
  lazy val `sh:IRIOrLiteral`: IRI = sh + "IRIOrLiteral"
  lazy val `sh:Literal`: IRI = sh + "Literal"
  lazy val `sh:NodeShape`: IRI = sh + "NodeShape"
  lazy val `sh:PropertyShape`: IRI = sh + "PropertyShape"
  lazy val `sh:Shape`: IRI = sh + "Shape"
  lazy val `sh:Schema`: IRI = sh + "Schema"
  lazy val `sh:ValidationReport`: IRI = sh + "ValidationReport"
  lazy val `sh:ValidationResult`: IRI = sh + "ValidationResult"
  lazy val `sh:Violation`: IRI = sh + "Violation"
  lazy val `sh:Warning`: IRI = sh + "Warning"

  lazy val `sh:and`: IRI = sh + "and"
  lazy val `sh:class`: IRI = sh + "class"
  lazy val `sh:closed`: IRI = sh + "closed"
  lazy val `sh:conforms`: IRI = sh + "conforms"
  lazy val `sh:datatype`: IRI = sh + "datatype"
  lazy val `sh:deactivated`: IRI = sh + "deactivated"
  lazy val `sh:description`: IRI = sh + "description"
  lazy val `sh:disjoint`: IRI = sh + "disjoint"
  lazy val `sh:equals`: IRI = sh + "equals"
  lazy val `sh:entailment`: IRI = sh + "entailment"
  lazy val `sh:flags`: IRI = sh + "flags"
  lazy val `sh:focusNode`: IRI = sh + "focusNode"
  lazy val `sh:group`: IRI = sh + "group"
  lazy val `sh:hasValue`: IRI = sh + "hasValue"
  lazy val `sh:ignoredProperties`: IRI = sh + "ignoredProperties"
  lazy val `sh:in`: IRI = sh + "in"
  lazy val `sh:languageIn`: IRI = sh + "languageIn"
  lazy val `sh:lessThan`: IRI = sh + "lessThan"
  lazy val `sh:lessThanOrEquals`: IRI = sh + "lessThanOrEquals"
  lazy val `sh:minCount`: IRI = sh + "minCount"
  lazy val `sh:maxCount`: IRI = sh + "maxCount"
  lazy val `sh:minInclusive`: IRI = sh + "minInclusive"
  lazy val `sh:minExclusive`: IRI = sh + "minExclusive"
  lazy val `sh:maxInclusive`: IRI = sh + "maxInclusive"
  lazy val `sh:maxExclusive`: IRI = sh + "maxExclusive"
  lazy val `sh:minLength`: IRI = sh + "minLength"
  lazy val `sh:maxLength`: IRI = sh + "maxLength"
  lazy val `sh:message`: IRI = sh + "message"
  lazy val `sh:name`: IRI = sh + "name"
  lazy val `sh:nodeKind`: IRI = sh + "nodeKind"
  lazy val `sh:node`: IRI = sh + "node"
  lazy val `sh:not`: IRI = sh + "not"
  lazy val `sh:or`: IRI = sh + "or"
  lazy val `sh:order`: IRI = sh + "order"
  lazy val `sh:path`: IRI = sh + "path"
  lazy val `sh:pattern`: IRI = sh + "pattern"
  lazy val `sh:property`: IRI = sh + "property"
  lazy val `sh:qualifiedMinCount`: IRI = sh + "qualifiedMinCount"
  lazy val `sh:qualifiedMaxCount`: IRI = sh + "qualifiedMaxCount"
  lazy val `sh:qualifiedValueShape`: IRI = sh + "qualifiedValueShape"
  lazy val `sh:qualifiedValueShapesDisjoint`: IRI = sh + "qualifiedValueShapesDisjoint"
  lazy val `sh:result`: IRI = sh + "result"
  lazy val `sh:resultPath`: IRI = sh + "resultPath"
  lazy val `sh:resultSeverity`: IRI = sh + "resultSeverity"
  lazy val `sh:resultMessage`: IRI = sh + "resultMessage"
  lazy val `sh:shapesGraph`: IRI = sh + "shapesGraph"
  lazy val `sh:severity`: IRI = sh + "severity"
  lazy val `sh:sourceConstraintComponent`: IRI = sh + "sourceConstraintComponent"
  lazy val `sh:sourceShape`: IRI = sh + "sourceShape"
  lazy val `sh:value`: IRI = sh + "value"
  lazy val `sh:targetNode`: IRI = sh + "targetNode"
  lazy val `sh:targetClass`: IRI = sh + "targetClass"
  lazy val `sh:targetSubjectsOf`: IRI = sh + "targetSubjectsOf"
  lazy val `sh:targetObjectsOf`: IRI = sh + "targetObjectsOf"
  lazy val `sh:text`: IRI = sh + "text"
  lazy val `sh:uniqueLang`: IRI = sh + "uniqueLang"
  lazy val `sh:xone`: IRI = sh + "xone"

  lazy val `owl:imports`: IRI = owl + "imports"

  lazy val defaultPrefixMap = PrefixMap(
    Map(
      Prefix("sh") -> sh,
      Prefix("rdf") -> rdf,
      Prefix("xsd") -> xsd,
      Prefix("rdfs") -> rdfs,
      Prefix("owl") -> owl
    ))

}
