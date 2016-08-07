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
  
  lazy val sh_ClosedShape = sh + "Closed"
  lazy val sh_Shape = sh + "Shape"
  lazy val sh_constraint = sh + "constraint"
  lazy val sh_OrConstraint = sh + "OrConstraint"
  lazy val sh_shapes = sh + "shapes"
  lazy val sh_minCount = sh + "minCount"
  lazy val sh_maxCount = sh + "maxCount"
  lazy val sh_in = sh + "in"
  lazy val sh_property = sh + "property"
  lazy val sh_predicate = sh + "predicate"
  lazy val sh_inverseProperty = sh + "inverseProperty"
  lazy val sh_PropertyConstraint = sh + "PropertyConstraint"
  lazy val sh_InversePropertyConstraint = sh + "InversePropertyConstraint"
  lazy val sh_nodeKind = sh + "nodeKind"
  lazy val sh_someOf = sh + "someOf"
  lazy val sh_oneOf = sh + "oneOf"
  lazy val sh_Schema = sh + "Schema"
  lazy val sh_datatype = sh + "datatype"
  lazy val sh_valueShape = sh + "valueShape"
  lazy val sh_targetNode = sh + "targetNode"
  lazy val sh_targetClass = sh + "targetClass"
  
  lazy val sh_text = sh + "text"
  

}