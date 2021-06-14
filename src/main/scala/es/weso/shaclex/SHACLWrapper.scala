package es.weso.shaclex

import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect._
import es.weso.schema._
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
  ): RDFReport = {
      val dataReader = new InputStreamReader(data)

      val cmp: IO[RDFReport] = for {
          res1 <- RDFAsJenaModel.fromReader(dataReader,options.dataFormat,options.base)
          res2 <- if (shapes == null) RDFAsJenaModel.empty
                  else RDFAsJenaModel.fromReader(new InputStreamReader(shapes),options.dataFormat,options.base)
          res3 <- RDFAsJenaModel.empty                  
          v <- (res1,res2, res3).tupled.use {
              case (rdf,shapes, builder) => for {
                prefixMap <- rdf.getPrefixMap
                schema <- Schemas.fromRDF(shapes, "SHACLEX")
                pm <- rdf.getPrefixMap
                result <- schema.validate(rdf,TargetDeclarations, builder)
              } yield result.validationReport
          }
      } yield v 
      cmp.unsafeRunSync()
    }

}