package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.scalalogging._
//import org.slf4j.LoggerFactory
// import es.weso.shacl._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import scala.concurrent.duration._
import es.weso.utils.FileUtils
import scala.util._
import java.nio.file._
import es.weso.shacl.RDF2Shacl
import es.weso.rdf.RDFReader
import java.io.File

object Main extends App with LazyLogging {

  override def main(args: Array[String]): Unit = {

   val opts = new MainOpts(args, errorDriver)
   opts.verify()

   val startTime = System.nanoTime()

   val validateOptions = for {
      rdf <- getRDFReader(opts)
      schema <- getSchema(opts,rdf)
    } yield (rdf,schema)

    validateOptions match {
      case Success((rdf,schema)) => {
        if (opts.showData()) {
          // If not specified uses the input schema format
          val outDataFormat = opts.outDataFormat.getOrElse(opts.dataFormat())
          println(rdf.serialize(outDataFormat))
        }
        if (opts.showSchema()) {
          // If not specified uses the input schema format
          val outSchemaFormat = opts.outSchemaFormat.getOrElse(opts.schemaFormat())
          schema.serialize(outSchemaFormat) match {
            case Success(str) => println(str)
            case Failure(e) => println(s"Error showing schema $schema with format $outSchemaFormat: $e")
          }
        }

        val result = if (opts.validate()) {
          schema.validate(rdf)
        } else
          Result.empty

        if (opts.validate() && opts.showResult()) {
          println(s"Result: \n${result.show(schema.pm)}")
        }

        if (opts.cnvEngine.isDefined) {
          println("Conversion between engines don't implemented yet")
        }
        if (opts.outputFile.isDefined) {
          val fileName = opts.outputFile()
          val str = result.show(schema.pm)
          FileUtils.writeFile(fileName, str)
        }

        if (opts.time()) {
          val endTime = System.nanoTime()
          val time : Long = endTime - startTime
          printTime("Time", opts, time)
        }

      }
      case Failure(e) => {
        println("Exception: " + e.getMessage())
      }
    }

  }

  def printTime(msg: String, opts: MainOpts, nanos: Long): Unit = {
    if (opts.time()) {
      val time = Duration(nanos, NANOSECONDS).toMillis
      println(f"$msg%s, $time%10d")
    }
  }

  private def errorDriver(e: Throwable, scallop: Scallop) = e match {
    case Help(s) => {
      println("Help: " + s)
      scallop.printHelp
      sys.exit(0)
    }
    case _ => {
      println("Error: %s".format(e.getMessage))
      scallop.printHelp
      sys.exit(1)
    }
  }

  def getRDFReader(opts: MainOpts): Try[RDFReader] = {
    if (opts.data.isDefined) {
      val path = Paths.get(opts.data())
      val rdf = RDFAsJenaModel.fromFile(path.toFile(),opts.dataFormat())
      rdf
    } else {
      logger.info("RDF Data option not specified")
      Success(RDFAsJenaModel.empty)
    }
  }

  def getSchema(opts: MainOpts, rdf: RDFReader): Try[Schema] = {
    if (opts.schema.isDefined) {
      val path = Paths.get(opts.schema())
      val schema = Schemas.fromFile(path.toFile(),
                       opts.schemaFormat(),
                       opts.engine(),
                       None)
      schema
    } else {
      logger.info("Schema not specified. Extracting schema from data")
      Schemas.fromRDF(rdf, opts.engine())
    }
  }

}

