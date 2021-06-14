package es.weso.shaclex

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex._
import es.weso.shapemaps._
import es.weso.shex.validator._
import cats.effect._
import cats.implicits._
import es.weso.utils.IOUtils._
import java.io.InputStream
import java.io.InputStreamReader
import cats.effect.unsafe.implicits.global

case class ResultValidation(resultShapeMap: ResultShapeMap, expectedShapeMap: ShapeMap)

// Wrapper to invoke ShEx from Java
object ShExWrapper {

  /**
   * Validate using ShEx
   * @param data RDF data
   * @param schema ShEx schema
   * @param ShapeMap
   * @param Validation options 
   **/
  def validate(
     data: InputStream, 
     schema: InputStream, 
     shapeMap: InputStream, 
     options: ShExsOptions
  ): ResultShapeMap = {
      val dataReader = new InputStreamReader(data)

      val cmp: IO[ResultShapeMap] = for {
          res1 <- RDFAsJenaModel.fromReader(dataReader,options.dataFormat,options.base)
          res2 <- RDFAsJenaModel.empty
          v <- (res1,res2).tupled.use {
              case (rdf,builder) => for {
                prefixMap <- rdf.getPrefixMap
                schema <- Schema.fromInputStream(schema, options.schemaFormat, options.base,None)
                resolved <- ResolvedSchema.resolve(schema, options.base)
                shapeMap <- fromES(
                  ShapeMap.fromInputStream(shapeMap, options.shapemapFormat,options.base, prefixMap,resolved.prefixMap
                  ).leftMap(es => es.toList.mkString("\n")))
                fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,prefixMap,schema.prefixMap)
                result <- Validator.validate(resolved,fixedShapeMap,rdf,builder,options.verbose)
                resultShapeMap <- result.toResultShapeMap
              } yield resultShapeMap
          }
      } yield v 
      cmp.unsafeRunSync()
    }

    /**
      * Validate RDF data using ShEx
      *
      * @param dataStr string containing RDF data
      * @param schemaStr string containing ShEx schema
      * @param shapeMapStr string containing shape map
      * @param options object representing validation options
      * @return resultShapeMap
      */
    def validate(
        dataStr: String, 
        schemaStr: String, 
        shapeMapStr: String, 
        options: ShExsOptions
    ): ResultShapeMap = {

      val cmp: IO[ResultShapeMap] = for {
          res1 <- RDFAsJenaModel.fromString(dataStr,options.dataFormat,options.base)
          res2 <- RDFAsJenaModel.empty
          v <- (res1,res2).tupled.use {
              case (rdf,builder) => for {
                prefixMap <- rdf.getPrefixMap      
                schema <- Schema.fromString(schemaStr, options.schemaFormat, options.base,None)
                resolved <- ResolvedSchema.resolve(schema, options.base)
                shapeMap <- fromES(
                  ShapeMap.fromString(shapeMapStr, options.shapemapFormat,options.base, prefixMap,resolved.prefixMap).leftMap(_.toString.mkString("\n"))
                )
                fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,prefixMap,schema.prefixMap)
                result <- Validator.validate(resolved,fixedShapeMap,rdf,builder,options.verbose)
                resultShapeMap <- result.toResultShapeMap
              } yield resultShapeMap
          }
      } yield v 
      cmp.unsafeRunSync()
    }

    /**
      * Validate RDF data using ShEx
      *
      * @param dataStr string containing RDF data
      * @param schemaStr string containing ShEx schema
      * @param node IRI of RDF node to check conformance
      * @param shape IRI of shape label target shape label
      * @param options object representing validation options
      * @return resultShapeMap
      */
    def validateNodeShape(
        dataStr: String, 
        schemaStr: String, 
        node: String, 
        shape: String,
        options: ShExsOptions
    ): ResultShapeMap = {
      validate(dataStr,schemaStr,s"<$node>@<$shape>",options)
    }


  /**
    * Validate an ontology and RDF data in Turtle format with a ShEx schema using a shapeMap
    * And returning a resultShapeMap and the result of parsing an expected shapeMap
    * 
    * Notice that this method is temporarily hardcoded to facilitate test-case creation
    *
    * @param ontologyStr String with the ontology contents in Turtle
    * @param dataStr String with RDF data contents in Turtle
    * @param schemaStr ShEx schema in Compact syntax
    * @param shapeMapStr ShapeMap in Compact syntax
    * @param expectedShapeMapStr Expected ShapeMap result
    * @return result of validation
    */
  def validateStrResultValidation(
    ontologyStr: String, 
    dataStr: String, 
    schemaStr: String, 
    shapeMapStr: String, 
    expectedShapeMapStr: String
    ): ResultValidation = {
    def cmp: IO[ResultValidation] = for {
      resultShapeMap <- validateStrIO(dataStr,ontologyStr,schemaStr,shapeMapStr)
      expectedShapeMap <- fromES(ShapeMap.fromString(expectedShapeMapStr, "Compact",None, resultShapeMap.nodesPrefixMap,resultShapeMap.shapesPrefixMap).leftMap(_.toString.mkString("\n")))        
     } yield ResultValidation(resultShapeMap, expectedShapeMap)
    cmp.unsafeRunSync()
  }


  def validateStrIO(
    dataStr: String, 
    ontologyStr: String, 
    schemaStr: String, 
    shapeMapStr: String): IO[ResultShapeMap] = for {
     res1 <- RDFAsJenaModel.fromString(ontologyStr,"TURTLE", None)
     res2 <- RDFAsJenaModel.fromString(dataStr,"TURTLE",None)
     res3 <- RDFAsJenaModel.empty
     v <- (res1,res2,res3).tupled.use {
      case (rdfOntology, rdfData, builder) => for {
          merged <- rdfOntology.merge(rdfData)
          prefixMap <- merged.getPrefixMap
          schema <- Schema.fromString(schemaStr, "SHEXC", None,None)
          resolved <- ResolvedSchema.resolve(schema, None)
          shapeMap <- fromES(ShapeMap.fromString(shapeMapStr, "Compact",None, prefixMap,resolved.prefixMap).leftMap(_.toString.mkString("\n")))
          fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,merged,prefixMap,resolved.prefixMap)
          result <- Validator.validate(resolved,fixedShapeMap,merged,builder,false)
          resultShapeMap <- result.toResultShapeMap
     } yield resultShapeMap
    }
   } yield v

  def validateStr(dataStr: String, ontologyStr: String, schemaStr: String, shapeMapStr: String): ResultShapeMap = {
    validateStrIO(dataStr,ontologyStr,schemaStr,shapeMapStr).unsafeRunSync()
  }


}