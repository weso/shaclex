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
import es.weso.rdf.RDFReader
import java.nio.file._

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
      schema <- getSchema(opts,rdf)
    } yield (rdf,schema)

    tryValidate match {
      case Success((reader,schema)) => {
        if (opts.show()) {
          println("Validation report:")
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
  
   def getSchema(opts: MainOpts, rdf: RDFReader): Try[Schema] = {
    if (opts.shacl.isDefined) {
      val path = Paths.get(opts.data())
      for {
        rdf <- RDFAsJenaModel.fromFile(path.toFile(),opts.shaclFormat())
        schema <- extractSchema(rdf)
      } yield schema
    } else {
      Failure(throw new Exception("Not supported yet extraction of shapes from RDF"))
    }
  }
   
  def extractSchema(rdf:RDFReader): Try[Schema] = {
    Failure(throw new Exception("Not yet supported conversion from RDF to Schema"))
  }

}
    
