package es.weso.shacl

import java.io.InputStream
import java.net.URI
import java.util.UUID
import org.topbraid.shacl.arq.SHACLFunctions
import org.topbraid.shacl.constraints.ModelConstraintValidator
import org.topbraid.shacl.vocabulary.SH
import org.topbraid.spin.arq.ARQFactory
import org.topbraid.spin.util.JenaUtil
import org.topbraid.shacl.util.ModelPrinter
import com.hp.hpl.jena.graph.Graph
import com.hp.hpl.jena.graph.compose.MultiUnion
import com.hp.hpl.jena.query.Dataset
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.util.FileUtils
import org.apache.jena.riot._
import java.io.ByteArrayOutputStream
 

object ShaclValidator {
  
  def validate(dataModel: Model, shapesModel: Model): (Model,Long) = {
    try {
      val shaclModel = JenaUtil.createDefaultModel()
      val is = getClass().getResourceAsStream("/etc/shacl.ttl")
      shaclModel.read(is, SH.BASE_URI, FileUtils.langTurtle)
      
      val combined = ModelFactory.createDefaultModel()
      combined.add(dataModel);
      combined.add(shapesModel);
      combined.add(shaclModel);
      
      SHACLFunctions.registerFunctions(combined)
      
      val shapesGraphURI = URI.create("urn:x-shacl-shapes-graph:" + UUID.randomUUID().toString())
      val dataset = ARQFactory.get.getDataset(dataModel)
      dataset.addNamedModel(shapesGraphURI.toString(), combined)
      
      val startTime = System.nanoTime()
      val results = ModelConstraintValidator.get.validateModel(dataset, shapesGraphURI, null, false, null)
      results.setNsPrefixes(shaclModel)
      val endTime = System.nanoTime()
      val time = endTime - startTime
      (results,time)
    } catch {
      case e: Exception => {
        val model = ModelFactory.createDefaultModel
        model.add(
            model.createResource(), 
            model.createProperty("exception"), 
            model.createLiteral(e.toString))
        (model,0)    
      }
    }
    
  }
  
  def validate(data: String, schema: String): (Model, Long) = {
    try {
      val dataModel = RDFDataMgr.loadModel(data)
      val shapesModel = RDFDataMgr.loadModel(schema)
      validate(dataModel,shapesModel)      
    } catch {
      case e: Exception => {
        val model = ModelFactory.createDefaultModel
        model.add(
            model.createResource(), 
            model.createProperty("exception"), 
            model.createLiteral(e.toString))
        (model,0)    
      }
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