package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.scalalogging._
// import es.weso.server._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import scala.concurrent.duration._
import es.weso.utils.FileUtils
import scala.util._
import java.nio.file._
import es.weso.rdf.RDFReader

object Main extends App with LazyLogging {
    try {
      run(args)
    } catch {
      case (e: Exception) => {
        println(s"Error: ${e.getMessage}")
      }
    }

  def run(args: Array[String]): Unit = {
    val opts = new MainOpts(args, errorDriver)
    opts.verify()

/*    if (opts.server()) {
      ShaclexServer.main(args)
    } */

    val baseFolder: Path = if (opts.baseFolder.isDefined) {
      Paths.get(opts.baseFolder())
    } else {
      Paths.get(".")
    }

    val startTime = System.nanoTime()

    val base = Some(FileUtils.currentFolderURL)

    val validateOptions: Either[String, (RDFReader, Schema, ValidationTrigger)] = for {
      rdf <- getRDFReader(opts, baseFolder)
      schema <- getSchema(opts, baseFolder, rdf)
      triggerName = opts.trigger.toOption.getOrElse(ValidationTrigger.default.name)
      shapeMapStr <- getShapeMapStr(opts)
      trigger <- ValidationTrigger.findTrigger(triggerName, shapeMapStr, base,
        opts.node.toOption, opts.shapeLabel.toOption,
        rdf.getPrefixMap(), schema.pm)
    } yield (rdf, schema, trigger)

    validateOptions match {
      case Left(e) => {
        println(s"Error: $e")
      }
      case Right((rdf, schema, trigger)) => {
        if (opts.showData()) {
          // If not specified uses the input schema format
          val outDataFormat = opts.outDataFormat.getOrElse(opts.dataFormat())
          rdf.serialize(outDataFormat) match {
            case Left(msg) => println(s"Error serializing to $outDataFormat: $msg")
            case Right(str) => println(str)
          }
        }
        if (opts.showSchema()) {
          // If not specified uses the input schema format
          val outSchemaFormat = opts.outSchemaFormat.getOrElse(opts.schemaFormat())
          schema.serialize(outSchemaFormat) match {
            case Right(str) => println(str)
            case Left(e) => println(s"Error showing schema $schema with format $outSchemaFormat: $e")
          }
        }

        if (opts.showShapeMap()) {
          println(s"Trigger shapemap: ${trigger.shapeMap}")
          println(s"ShapeMap: ${trigger.shapeMap.serialize(opts.outShapeMapFormat())}")
          println(s"Trigger json: ${trigger.toJson.spaces2}")
        }

        val result = schema.validate(rdf, trigger)

        if (opts.showLog()) {
          logger.info("Show log info = true")
          logger.info(s"JSON result: ${result.toJsonString2spaces}")
        }

        if (opts.showResult() || opts.outputFile.isDefined) {
          val resultSerialized = result.serialize(opts.resultFormat())
          if (opts.showResult()) println(resultSerialized)
          if (opts.outputFile.isDefined)
            FileUtils.writeFile(opts.outputFile(), resultSerialized)
        }

        if (opts.showValidationReport()) {
          val vr = result.validationReport
          (for {
            rdf <- vr
            str <- rdf.serialize(opts.validationReportFormat())
          } yield str).fold(
            e => println(s"Error: $e"),
            println(_)
          )
        }

        if (opts.cnvEngine.isDefined) {
          logger.error("Conversion between engines don't implemented yet")
        }

        if (opts.time()) {
          val endTime = System.nanoTime()
          val time: Long = endTime - startTime
          printTime("Time", opts, time)
        }

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

  def getShapeMapStr(opts: MainOpts): Either[String, String] = {
    if (opts.shapeMap.isDefined) {
      // val shapeMapFormat = opts.shapeMapFormat.toOption.getOrElse("COMPACT")
      for {
        // TODO: Allow different shapeMap formats
        content <- FileUtils.getContents(opts.shapeMap())
      } yield content.toString
    } else Right("")
  }

  def getRDFReader(opts: MainOpts, baseFolder: Path): Either[String, RDFReader] = {
    val base = Some(FileUtils.currentFolderURL)
    if (opts.data.isDefined) {
      val path = baseFolder.resolve(opts.data())
      for {
        rdf <- RDFAsJenaModel.fromFile(path.toFile(), opts.dataFormat(), base)
      } yield {
        if (opts.inference.isDefined) {
          rdf.applyInference(opts.inference()) match {
            case Right(newRdf) => newRdf
            case Left(msg) => {
              logger.info(s"Error applying inference: $msg")
              rdf
            }
          }
        } else rdf
      }
    } else {
      logger.info("RDF Data option not specified")
      Right(RDFAsJenaModel.empty)
    }
  }

  def getSchema(opts: MainOpts, baseFolder: Path, rdf: RDFReader): Either[String, Schema] = {
    val base = Some(FileUtils.currentFolderURL)
    if (opts.schema.isDefined) {
      val path = baseFolder.resolve(opts.schema())
      val schema = Schemas.fromFile(path.toFile(), opts.schemaFormat(), opts.engine(), base)
      schema
    } else {
      logger.info("Schema not specified. Extracting schema from data")
      Schemas.fromRDF(rdf, opts.engine())
    }
    //  }

  }
}

