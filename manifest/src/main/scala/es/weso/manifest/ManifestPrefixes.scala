package es.weso.manifest

import es.weso.rdf.nodes.IRI

object ManifestPrefixes {

  lazy val mf = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#")
  lazy val qt = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-query#")
  lazy val sht = IRI("http://www.w3.org/ns/shacl/test-suite#")
  lazy val dc = IRI("http://purl.org/dc/elements/1.1/")
  lazy val dawgt = IRI("http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#")
  lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  lazy val mf_Manifest = mf.add("Manifest")
  lazy val mf_entries = mf.add("entries")
  lazy val mf_include = mf.add("include")
  lazy val mf_name = mf.add("name")
  lazy val mf_action = mf.add("action")
  lazy val mf_result = mf.add("result")
  lazy val mf_status = mf.add("status")
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
  lazy val sht_shape = sht.add("shape")
  lazy val sht_Validate = sht.add("Validate")
  lazy val sht_ValidationTest = sht.add("ValidationTest")
  lazy val sht_ValidationFailure = sht.add("ValidationFailure")
  lazy val sht_WellFormedSchema = sht.add("WellFormedSchema")
  lazy val sht_NonWellFormedSchema = sht.add("NonWellFormedSchema")
  lazy val sht_MatchNodeShape = sht.add("MatchNodeShape")
  lazy val sht_ConvertSchemaSyntax = sht.add("ConvertSchemaSyntax")
  lazy val sht_SHACLC = sht.add("SHACLC")
  lazy val sht_TURTLE = sht.add("TURTLE")
  
  
}