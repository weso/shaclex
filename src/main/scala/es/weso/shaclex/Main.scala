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
import util._
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
          println(s"Data(${opts.dataFormat()}):\n${rdf.serialize(opts.dataFormat())}")
        }
        if (opts.showSchema()) {
          schema.serialize(opts.schemaFormat()) match {
            case Success(str) => println(s"Schema(${opts.schemaFormat()}):\n$str")
            case Failure(e) => println(s"Error showing schema $schema: $e")
          }
        }

        val result = schema.validate(rdf)
        logger.info(s"Result: ${result.show(schema.pm)}")

        if (opts.outputFile.get.isDefined) {
          val fileName = opts.outputFile.get.get
          val str = "" //TODO: str should contain the generated report
          logger.info("Not implemented file report generation yet")
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
      logger.info(s"RDF obtained: $rdf")
      rdf
    } else {
      logger.info("RDF Data option not specified")
      Success(RDFAsJenaModel.empty)
    }
  }

  def getSchema(opts: MainOpts, rdf: RDFReader): Try[Schema] = {
    if (opts.schema.isDefined) {
      logger.info(s"Schema specified: Extracting schema from ${opts.schema()}")
      val path = Paths.get(opts.schema())
      val schema = Schemas.fromFile(path.toFile(),
                       opts.schemaFormat(),
                       opts.engine(),
                       None)
      logger.info(s"Schema $schema")
      schema
    } else {
      logger.info("Schema not specified. Extracting schema from data")
      Schemas.fromRDF(rdf, opts.engine())
    }
  }

  /*def getShacl(file: File, format: String): Try[Schema] = {
    format match {
      case "TURTLE" => for {
        rdf <- RDFAsJenaModel.fromFile(file,format)
        schema <- extractSchema(rdf)
      } yield (schema)
      case _ => Failure(new Exception("Unsupported format: " + format))
    }
  }*/

/*  def extractSchema(rdf:RDFReader): Try[Schema] = {
    RDF2Shacl.getShacl(rdf)
  } */

}

