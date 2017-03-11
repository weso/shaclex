package es.weso.shacl

import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES._
import es.weso.rdf._

object SHACLPrefixes {

  def add(iri: IRI, str: String): IRI = {
    iri + str
  }

  lazy val sh = IRI("http://www.w3.org/ns/shacl#")

  lazy val sh_IRI: IRI = sh + "IRI"
  lazy val sh_Literal: IRI = sh + "Literal"
  lazy val sh_BlankNode: IRI = sh + "BlankNode"
  lazy val sh_IRIOrLiteral: IRI = sh + "IRIOrLiteral"
  lazy val sh_BlankNodeOrIRI: IRI = sh +  "BlankNodeOrIRI"
  lazy val sh_BlankNodeOrLiteral: IRI = sh +  "BlankNodeOrLiteral"

  lazy val sh_Shape: IRI = sh + "Shape"
  lazy val sh_NodeShape: IRI = sh + "NodeShape"
  lazy val sh_PropertyShape: IRI = sh + "PropertyShape"
  lazy val sh_property: IRI = sh + "property"
  lazy val sh_path: IRI = sh + "path"
  lazy val sh_inversePath: IRI = sh + "inversePath"
  lazy val sh_alternativePath: IRI = sh + "alternativePath"
  lazy val sh_zeroOrMorePath: IRI = sh + "zeroOrMorePath"
  lazy val sh_zeroOrOnePath: IRI = sh + "zeroOrOnePath"
  lazy val sh_oneOrMorePath: IRI = sh + "oneOrMorePath"

  lazy val sh_class : IRI= sh + "class"
  lazy val sh_datatype : IRI= sh + "datatype"
  lazy val sh_nodeKind : IRI= sh + "nodeKind"
  lazy val sh_minCount : IRI= sh + "minCount"
  lazy val sh_maxCount : IRI= sh + "maxCount"
  lazy val sh_minInclusive : IRI= sh + "minInclusive"
  lazy val sh_minExclusive : IRI= sh + "minExclusive"
  lazy val sh_maxInclusive : IRI= sh + "maxInclusive"
  lazy val sh_maxExclusive : IRI= sh + "maxExclusive"
  lazy val sh_minLength : IRI= sh + "minLength"
  lazy val sh_maxLength : IRI= sh + "maxLength"
  lazy val sh_pattern : IRI= sh + "pattern"
  lazy val sh_flags : IRI= sh + "flags"
  lazy val sh_languageIn : IRI = sh + "languageIn"
  lazy val sh_uniqueLang : IRI = sh + "uniqueLang"
  lazy val sh_equals : IRI= sh + "equals"
  lazy val sh_disjoint : IRI= sh + "disjoint"
  lazy val sh_lessThan : IRI= sh + "lessThan"
  lazy val sh_lessThanOrEquals : IRI= sh + "lessThanOrEquals"
  lazy val sh_not : IRI= sh + "not"
  lazy val sh_or : IRI= sh + "or"
  lazy val sh_and : IRI= sh + "and"
  lazy val sh_xone : IRI= sh + "xone"
  lazy val sh_node : IRI= sh + "node"
  lazy val sh_qualifiedValueShape : IRI = sh + "qualifiedValueShape"
  lazy val sh_qualifiedValueShapesDisjoint : IRI = sh + "qualifiedValueShapesDisjoint"

  lazy val sh_qualifiedMinCount : IRI = sh + "qualifiedMinCount"
  lazy val sh_qualifiedMaxCount : IRI = sh + "qualifiedMaxCount"
  lazy val sh_closed : IRI = sh + "closed"
  lazy val sh_ignoredProperties : IRI = sh + "ignoredProperties"
  lazy val sh_hasValue : IRI = sh + "hasValue"
  lazy val sh_in : IRI = sh + "in"
  lazy val sh_name : IRI = sh + "name"
  lazy val sh_description : IRI = sh + "description"

  lazy val sh_Schema : IRI = sh + "Schema"
  lazy val sh_targetNode : IRI = sh + "targetNode"
  lazy val sh_targetClass : IRI = sh + "targetClass"
  lazy val sh_targetSubjectsOf : IRI = sh + "targetSubjectsOf"
  lazy val sh_targetObjectsOf : IRI = sh + "targetObjectsOf"

  lazy val sh_text : IRI = sh + "text"

  lazy val defaultPrefixMap = PrefixMap(
    Map(Prefix("sh") -> sh,
        Prefix("rdf") -> rdf,
        Prefix("xsd") -> xsd,
        Prefix("rdfs") -> rdfs
    )
  )

}
