package es.weso.shextest.manifest

import es.weso.rdf.nodes.IRI

object ManifestPrefixes {

  lazy val mf = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")
  lazy val qt = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-query#")
  lazy val sht = IRI("http://www.w3.org/ns/shacl/test-suite#")
  lazy val dc = IRI("http://purl.org/dc/elements/1.1/")
  lazy val dawgt = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val rdf = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  lazy val sh = IRI("http://www.w3.org/ns/shacl#")
  lazy val sx = IRI("https://shexspec.github.io/shexTest/ns#")

  lazy val mf_Manifest = mf.add("Manifest")
  lazy val mf_entries = mf.add("entries")
  lazy val mf_include = mf.add("include")
  lazy val mf_name = mf.add("name")
  lazy val mf_action = mf.add("action")
  lazy val mf_result = mf.add("result")
  lazy val mf_status = mf.add("status")

  lazy val rdf_type = rdf + "type"

  lazy val sh_focusNode = sh + "focusNode"
  lazy val sh_path = sh + "path"
  lazy val sh_severity = sh + "severity"
  lazy val sh_sourceConstraintComponent = sh + "sourceConstraintComponent"
  lazy val sh_sourceShape = sh + "sourceConstraintComponent"
  lazy val sh_value = sh + "value"
  lazy val sh_result = sh + "result"
  lazy val sh_ValidationReport = sh + "ValidationReport"

  lazy val rdfs_label = rdfs.add("label")
  lazy val rdfs_comment = rdfs.add("comment")

  lazy val sht_proposed = sht.add("proposed")
  lazy val sht_approved = sht.add("approved")
  lazy val sht_rejected = sht.add("rejected")
  lazy val sht_specRef = sht.add("specRef")
  lazy val sht_schema = sht.add("schema")
  lazy val sht_schema_format = sht.add("schema-format")
  lazy val sht_data = sht.add("data")
  lazy val sht_data_format = sht.add("data-format")
  lazy val sht_schema_output_format = sht.add("schema-output-format")
  lazy val sht_node = sht.add("node")
  lazy val sht_focus = sht.add("focus")
  lazy val sht_map = sht.add("map")
  lazy val sht_triggerMode = sht.add("triggerMode")
  lazy val sht_shape = sht.add("shape")
  lazy val sht_details = sht + "details"
  lazy val sht_pair = sht + "pair"
  lazy val sht_trait = sht + "trait"
  lazy val sht_validatedPairs = sht + "validatedPairs"
  lazy val sht_Valid = sht + "Valid"
  lazy val sht_NotValid = sht + "NotValid"
  lazy val sht_Validate = sht.add("Validate")
  lazy val sht_NegativeSyntax = sht.add("NegativeSyntax")
  lazy val sht_NegativeStructure = sht.add("NegativeStructure")
  lazy val sht_RepresentationTest = sht.add("RepresentationTest")
  lazy val sht_ValidationTest = sht.add("ValidationTest")
  lazy val sht_ValidationFailure = sht.add("ValidationFailure")
  lazy val sht_Greedy = sht.add("Greedy")
  lazy val sht_shapeMap = sht + "shapeMap"
  lazy val sht_shapeExterns = sht + "shapeExterns"
  lazy val sht_resultShapeMap = sht + "resultShapeMap"
  lazy val sht_ResultShapeMap = sht + "ResultShapeMap"

  lazy val sx_shex = sx + "shex"
  lazy val sx_json = sx + "json"
  lazy val sx_ttl = sx + "ttl"

  lazy val sht_WellFormedSchema = sht.add("WellFormedSchema")
  lazy val sht_NonWellFormedSchema = sht.add("NonWellFormedSchema")
  lazy val sht_MatchNodeShape = sht.add("MatchNodeShape")
  lazy val sht_ConvertSchemaSyntax = sht.add("ConvertSchemaSyntax")
  lazy val sht_SHACLC = sht.add("SHACLC")
  lazy val sht_TURTLE = sht.add("TURTLE")

}
