package es.weso.shacl

import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES._
import es.weso.rdf._

object SHACLPrefixes {

//  lazy val sh = IRI("http://www.w3.org/ns/shacl#")

  lazy val sh_BlankNode: IRI = sh + "BlankNode"
  lazy val sh_BlankNodeOrIRI: IRI = sh + "BlankNodeOrIRI"
  lazy val sh_BlankNodeOrLiteral: IRI = sh + "BlankNodeOrLiteral"
  lazy val sh_Info: IRI = sh + "Info"
  lazy val sh_IRI: IRI = sh + "IRI"
  lazy val sh_IRIOrLiteral: IRI = sh + "IRIOrLiteral"
  lazy val sh_Literal: IRI = sh + "Literal"
  lazy val sh_NodeShape: IRI = sh + "NodeShape"
  lazy val sh_PropertyShape: IRI = sh + "PropertyShape"
  lazy val sh_Shape: IRI = sh + "Shape"
  lazy val sh_Schema: IRI = sh + "Schema"
  lazy val sh_ValidationReport: IRI = sh + "ValidationReport"
  lazy val sh_ValidationResult: IRI = sh + "ValidationResult"
  lazy val sh_Violation: IRI = sh + "Violation"
  lazy val sh_Warning: IRI = sh + "Warning"

  lazy val sh_and: IRI = sh + "and"
  lazy val sh_class: IRI = sh + "class"
  lazy val sh_closed: IRI = sh + "closed"
  lazy val sh_conforms: IRI = sh + "conforms"
  lazy val sh_datatype: IRI = sh + "datatype"
  lazy val sh_deactivated: IRI = sh + "deactivated"
  lazy val sh_description: IRI = sh + "description"
  lazy val sh_disjoint: IRI = sh + "disjoint"
  lazy val sh_equals: IRI = sh + "equals"
  lazy val sh_entailment: IRI = sh + "entailment"
  lazy val sh_flags: IRI = sh + "flags"
  lazy val sh_focusNode: IRI = sh + "focusNode"
  lazy val sh_group: IRI = sh + "group"
  lazy val sh_hasValue: IRI = sh + "hasValue"
  lazy val sh_ignoredProperties: IRI = sh + "ignoredProperties"
  lazy val sh_in: IRI = sh + "in"
  lazy val sh_languageIn: IRI = sh + "languageIn"
  lazy val sh_lessThan: IRI = sh + "lessThan"
  lazy val sh_lessThanOrEquals: IRI = sh + "lessThanOrEquals"
  lazy val sh_minCount: IRI = sh + "minCount"
  lazy val sh_maxCount: IRI = sh + "maxCount"
  lazy val sh_minInclusive: IRI = sh + "minInclusive"
  lazy val sh_minExclusive: IRI = sh + "minExclusive"
  lazy val sh_maxInclusive: IRI = sh + "maxInclusive"
  lazy val sh_maxExclusive: IRI = sh + "maxExclusive"
  lazy val sh_minLength: IRI = sh + "minLength"
  lazy val sh_maxLength: IRI = sh + "maxLength"
  lazy val sh_message: IRI = sh + "message"
  lazy val sh_name: IRI = sh + "name"
  lazy val sh_nodeKind: IRI = sh + "nodeKind"
  lazy val sh_node: IRI = sh + "node"
  lazy val sh_not: IRI = sh + "not"
  lazy val sh_or: IRI = sh + "or"
  lazy val sh_order: IRI = sh + "order"
  lazy val sh_path: IRI = sh + "path"
  lazy val sh_pattern: IRI = sh + "pattern"
  lazy val sh_property: IRI = sh + "property"
  lazy val sh_qualifiedMinCount: IRI = sh + "qualifiedMinCount"
  lazy val sh_qualifiedMaxCount: IRI = sh + "qualifiedMaxCount"
  lazy val sh_qualifiedValueShape: IRI = sh + "qualifiedValueShape"
  lazy val sh_qualifiedValueShapesDisjoint: IRI = sh + "qualifiedValueShapesDisjoint"
  lazy val sh_result: IRI = sh + "result"
  lazy val sh_resultPath: IRI = sh + "resultPath"
  lazy val sh_resultSeverity: IRI = sh + "resultSeverity"
  lazy val sh_resultMessage: IRI = sh + "resultMessage"
  lazy val sh_shapesGraph: IRI = sh + "shapesGraph"
  lazy val sh_severity: IRI = sh + "severity"
  lazy val sh_sourceConstraintComponent: IRI = sh + "sourceConstraintComponent"
  lazy val sh_sourceShape: IRI = sh + "sourceShape"
  lazy val sh_value: IRI = sh + "value"
  lazy val sh_targetNode: IRI = sh + "targetNode"
  lazy val sh_targetClass: IRI = sh + "targetClass"
  lazy val sh_targetSubjectsOf: IRI = sh + "targetSubjectsOf"
  lazy val sh_targetObjectsOf: IRI = sh + "targetObjectsOf"
  lazy val sh_text: IRI = sh + "text"
  lazy val sh_uniqueLang: IRI = sh + "uniqueLang"
  lazy val sh_xone: IRI = sh + "xone"

  lazy val owl_imports: IRI = owl + "imports"

  lazy val defaultPrefixMap = PrefixMap(
    Map(
      Prefix("sh") -> sh,
      Prefix("rdf") -> rdf,
      Prefix("xsd") -> xsd,
      Prefix("rdfs") -> rdfs,
      Prefix("owl") -> owl
    ))

}
