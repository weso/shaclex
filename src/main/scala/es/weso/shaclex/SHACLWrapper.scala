package es.weso.shaclex

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl._
import es.weso.shapemaps._
import es.weso.shacl.validator._
import cats.effect._
import cats.implicits._
import es.weso.utils.IOUtils._
import java.io.InputStream
import java.io.InputStreamReader
import cats.effect.unsafe.implicits.global
import es.weso.shacl.report.ValidationReport

// Wrapper to invoke SHACL from Java
object SHACLWrapper {

  /**
   * Validate using SHACL
   * @param data RDF data
   * @param shapes Shapes graph, if null the shapes graph is assumed to be embedded in RDF data
   * @param Validation options 
   **/
  def validate(
     data: InputStream, 
     shapes: InputStream, 
     options: SHACLsOptions
  ): ValidationReport = {
      val dataReader = new InputStreamReader(data)

      val cmp: IO[ResultShapeMap] = for {
          res1 <- RDFAsJenaModel.fromReader(dataReader,options.dataFormat,options.base)
          res2 <- if (shapes == null) RDFAsJenaModel.empty
                  else RDFAsJenaModel.fromReader(new InputStreamReader(shapes),options.dataFormat,options.base)
          v <- (res1,res2).tupled.use {
              case (rdf,shapes) => for {
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

}