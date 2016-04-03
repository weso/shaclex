package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shacl._
import es.weso.rdf.jena.RDFAsJenaModel
import org.apache.jena.riot._
import scala.concurrent.duration._
import org.slf4j._
import org.apache.log4j._
import es.weso.utils.FileUtils
import es.weso.utils.TryUtils
import util._
import es.weso.rdf.{ PrefixMap, RDFReader }
import java.nio.file._
import es.weso.shacl.converter.RDF2Schema
import java.io.File

object Main extends App {
  override def main(args: Array[String]): Unit = {

    lazy val log = LogManager.getRootLogger
    val appenders = log.getAllAppenders
    val appender: ConsoleAppender = log.getAppender("stdout").asInstanceOf[ConsoleAppender]

    val error = Level.ERROR
    log.setLevel(error)
    appender.setThreshold(error)

    val opts = new MainOpts(args, errorDriver)
    opts.verify()
    val startTime = System.nanoTime()


    val tryValidate = for {
      rdf <- getRDFReader(opts)
      (schema,pm) <- getSchema(opts,rdf)
    } yield (rdf,schema,pm)

    tryValidate match {
      case Success((reader,schema,pm)) => {
        if (opts.show()) {
          println("Schema:" + schema.serialize("TURTLE"))
        }

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
      Success(RDFAsJenaModel.empty)
    }
  }
  
   def getSchema(opts: MainOpts, rdf: RDFReader): Try[(Schema,PrefixMap)] = {
    if (opts.shacl.isDefined) {
      val path = Paths.get(opts.shacl())
      for {
        rdf <- RDFAsJenaModel.fromFile(path.toFile(),opts.shaclFormat())
        (schema,pm) <- extractSchema(rdf)
      } yield (schema,pm)
    } else {
      extractSchema(rdf)
    }
  }

  def getShacl(file: File, format: String): Try[(Schema,PrefixMap)] = {
    format match {
      case "TURTLE" => for {
        rdf <- RDFAsJenaModel.fromFile(file,format)
        (schema,pm) <- extractSchema(rdf)
      } yield (schema,pm)
      case _ => Failure(throw new Exception("Unsupported format: " + format))
    }
  }
  
  def extractSchema(rdf:RDFReader): Try[(Schema,PrefixMap)] = {
    RDF2Schema.rdf2Schema(rdf)
  }

}
    
