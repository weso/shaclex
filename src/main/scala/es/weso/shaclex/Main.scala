package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory
import es.weso.shacl._
import es.weso.rdf.jena.RDFAsJenaModel
import scala.concurrent.duration._
import es.weso.utils.FileUtils
import util._
import java.nio.file._
import es.weso.shacl.RDF2Shacl
import es.weso.rdf.RDFReader
import java.io.File

object Main extends App {

  val log = Logger(LoggerFactory.getLogger("name"))

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
        if (opts.show()) {
          println("Schema:" + schema.serialize("TURTLE"))
        }

        val validator = Validator(schema)
        val validated = validator.validateAll(rdf)
        println(s"Result: ${validated.show}")

        if (opts.outputFile.get.isDefined) {
          val fileName = opts.outputFile.get.get
          val str = "" //TODO: str should contain the generated report
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
    case Help(s) =>
      println("Help: " + s)
      scallop.printHelp
      sys.exit(0)
    case _ =>
      println("Error: %s".format(e.getMessage))
      scallop.printHelp
      sys.exit(1)
  }

  def getRDFReader(opts: MainOpts): Try[RDFReader] = {
    if (opts.data.isDefined) {
      val path = Paths.get(opts.data())
      RDFAsJenaModel.fromFile(path.toFile(),opts.dataFormat())
    } else {
      log.info("RDF Data option not specified")
      Success(RDFAsJenaModel.empty)
    }
  }

  def getSchema(opts: MainOpts, rdf: RDFReader): Try[Schema] = {
    if (opts.shacl.isDefined) {
      val path = Paths.get(opts.shacl())
      for {
        shaclRdf <- RDFAsJenaModel.fromFile(path.toFile(),opts.shaclFormat())
        schema <- extractSchema(rdf)
      } yield schema
    } else {
      log.info("Schema not specified. Extracting schema from data")
      extractSchema(rdf)
    }
  }

  def getShacl(file: File, format: String): Try[Schema] = {
    format match {
      case "TURTLE" => for {
        rdf <- RDFAsJenaModel.fromFile(file,format)
        schema <- extractSchema(rdf)
      } yield (schema)
      case _ => Failure(new Exception("Unsupported format: " + format))
    }
  }

  def extractSchema(rdf:RDFReader): Try[Schema] = {
    RDF2Shacl.getShacl(rdf)
  }

}

