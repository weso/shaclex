package es.weso.shacl

import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.util.FileUtils
import org.apache.jena.riot._
import java.io.ByteArrayOutputStream
import es.weso.rdf.jena.RDFAsJenaModel
 

object ShExValidator {
  
  def validate(schemaFile: String, dataModel: Model, explain: Boolean): (Boolean,Long) = {
  
    // TODO: Error checking in case of bad schema
    lazy val trySchema = Schema.fromFile(schemaFile,"SHEXC",None)
    if (trySchema.isFailure) {
      if (explain) {
        println("Error loading schema: " + trySchema.failed.get.getMessage)
      }
      (false,-1)
    } else {
     val (webindexSchema,_) = trySchema.get
     val rdf = RDFAsJenaModel(dataModel)
     val validator = ShaclMatcher(webindexSchema,rdf)
     val startTime = System.nanoTime()
     val result = validator.validate
     val endTime = System.nanoTime()
     val time = endTime - startTime
     if (result.isFailure && explain) {
       println("Not valid. Result:" + result)
     }
     (result.isValid,time)
    }
  }
  
  def result2Str(m: Model): String = {
    if (m.size == 0) {
      "Validated!"
    } else {
      model2Str(m)
    }
  }
  
  def model2Str(m: Model): String = {
    val out = new ByteArrayOutputStream
    RDFDataMgr.write(out,m,RDFFormat.TURTLE_PRETTY)
    out.toString       
  }


}