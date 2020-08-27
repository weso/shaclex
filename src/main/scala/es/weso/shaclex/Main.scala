package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.scalalogging._
import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.schemaInfer.SchemaInfer
import es.weso.shapeMaps.NodeSelector

import scala.io.Source
// import es.weso.server._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import scala.concurrent.duration._
import es.weso.utils.FileUtils
import scala.util._
import java.nio.file._
import es.weso.rdf.RDFReader
import cats.data.EitherT
import cats.effect._
import cats._
// import cats.implicits._

object Main extends IOApp with LazyLogging {

  type ESIO[A] = EitherT[IO, String, A]

  private def fromUnit(x: =>Unit): ESIO[Unit] = 
    EitherT.liftF[IO,String,Unit](IO(x))

  private def fromEither[A](e: Either[String,A]): ESIO[A] =
    EitherT.fromEither[IO](e)

  // private def printlnIO(str: String): ESIO[Unit] = EitherT.liftF[IO,String,Unit](IO { println(str) })

  private def whenA[A](x: Boolean, v: ESIO[Unit]): ESIO[Unit] = {
    Applicative[ESIO].whenA(x)(v)
  }

  val relativeBase: Option[IRI] = Some(IRI("internal://base/"))
  val relativeBaseStr = relativeBase.map(_.str)

  private def getBaseFolder(opts: MainOpts): IO[Path] = 
   IO(if (opts.baseFolder.isDefined) {
    Paths.get(opts.baseFolder())
   } else {
    Paths.get(".")
   })

  private def doShExTest(opts:MainOpts): IO[Unit] =  
   if (opts.testShEx.isDefined) {
    IO(ShExTestRunner.run(opts.testShEx()))
   } else IO(())

   private def doShapeInfer(baseFolder: Path, opts:MainOpts): IO[Unit] =  
   if (opts.shapeInfer()) {
    shapeInfer(opts,baseFolder)
   } else IO(())

   private def doShowTime(startTime: Long, opts:MainOpts): IO[Unit] =
    if (opts.time()) IO {
    val endTime = System.nanoTime()
    val time: Long = endTime - startTime
    printTime("Time", opts, time)
   } else IO(())

   def run(args: List[String]): IO[ExitCode] = {
    val opts = new MainOpts(args, errorDriver)
    opts.verify()

    if (args.length==0) for {
      _ <- IO { opts.printHelp() }
    } yield ExitCode.Error
    else for {
     _ <- doShExTest(opts) 
     baseFolder <- getBaseFolder(opts) 
     startTime <- IO(System.nanoTime())
     _ <- doShapeInfer(baseFolder,opts)
     _ <- doProcess(baseFolder, opts)
     _ <- doShowTime(startTime, opts)
    } yield ExitCode.Success     
  }

  private def doProcess(baseFolder: Path, opts:MainOpts): IO[Unit] = {
    val e: EitherT[IO, String, Unit] = for {
    rdf <- getRDFReader(opts, baseFolder)
    schema <- getSchema(opts, baseFolder, rdf)
    triggerName = opts.trigger.toOption.getOrElse(ValidationTrigger.default.name)
    shapeMapStr <- getShapeMapStr(opts, baseFolder)
    trigger <- EitherT.fromEither[IO](ValidationTrigger.findTrigger(triggerName, shapeMapStr, relativeBaseStr,
        opts.node.toOption, opts.shapeLabel.toOption,
        rdf.getPrefixMap(), schema.pm))
    _ <- doShowData(opts,rdf)    
    _ <- doShowSchema(opts,schema)
    _ <- doShowShapeMap(opts, trigger)
    _ <- doShowClingo(opts,rdf,schema,trigger)
    _ <- whenA(opts.validate(), doValidation(opts, rdf, schema, trigger))
  } yield ()
   e.fold(e => for { 
    _ <- IO(println(s"Error: $e"))
   } yield (), 
   _ => IO.pure(())
   )
  }

  private def doValidation(opts: MainOpts, rdf: RDFReader, schema: Schema, trigger: ValidationTrigger): ESIO[Unit] = for {
    result <- EitherT.liftF(schema.validate(rdf, trigger))
    _ <- doShowResult(opts,result)
    _ <- doShowValidationReport(opts,result)
  } yield ()

  private def doShowData(opts:MainOpts, rdf:RDFReader): EitherT[IO, String, Unit] = {
    if (opts.showData()) {
      // If not specified uses the input schema format
      val outDataFormat = opts.outDataFormat.getOrElse(opts.dataFormat())
      EitherT.liftF(for { 
        str <- rdf.serialize(outDataFormat, relativeBase)
        _ <- IO(println(str))
      } yield ())

      /*match {
        case Left(msg) => fromUnit(println(s"Error serializing to $outDataFormat: $msg"))
        case Right(str) => fromUnit(println(str))
      }*/
    } else {
      EitherT.pure[IO,String](())
    }
  }

  private def doShowSchema(opts:MainOpts, schema:Schema): EitherT[IO,String,Unit] = {
    if (opts.showSchema() || opts.outSchemaFile.isDefined) {
      for {
        // _ <- printlnIO(s"Schema: ${schema}\n")
        str <- schema.convert(opts.outSchemaFormat.toOption,opts.outEngine.toOption,relativeBase)
        _ <- whenA(opts.showSchema(), fromUnit(println(str)))
        _ <- whenA(opts.outSchemaFile.isDefined, fromUnit(FileUtils.writeFile(opts.outSchemaFile(), str)))
      } yield ()
    } else {
      done
    }

  }

  private def done: EitherT[IO,String,Unit] = EitherT.pure[IO,String](())
  private def err[A](msg: String): ESIO[A] = fromIO(IO.raiseError(new RuntimeException(msg)))


  private def doShowShapeMap(opts:MainOpts, trigger: ValidationTrigger): ESIO[Unit] = 
   whenA(opts.showShapeMap(), 
     fromUnit(println(s"ShapeMap: ${trigger.shapeMap.serialize(opts.outShapeMapFormat(),relativeBase)}"))
   )

  private def doShowClingo(opts: MainOpts, rdf: RDFReader, schema: Schema, trigger: ValidationTrigger): ESIO[Unit] = 
    if (opts.clingoFile.isDefined || opts.showClingo()) for {
      str <- schema.toClingo(rdf, trigger.shapeMap).leftMap(e => s"Error converting to clingo: $e")
      _ <- whenA(opts.showClingo(),fromUnit(println(s"$str")))
      _ <- whenA(opts.clingoFile.isDefined, fromUnit(FileUtils.writeFile(opts.clingoFile(), str)))
    } yield (())
    else done

  private def doShowResult(opts: MainOpts, result: Result): ESIO[Unit] = 
  if (opts.showResult() || opts.outputFile.isDefined) {
    val resultSerialized = result.serialize(opts.resultFormat(),relativeBase)
    for {
      _ <- whenA(opts.showResult(),fromUnit(println(resultSerialized)))
      _ <- whenA(opts.outputFile.isDefined,fromUnit(FileUtils.writeFile(opts.outputFile(), resultSerialized)))
    } yield (())
  } else done

  private def fromIO[A](io: IO[A]): ESIO[A] = EitherT.liftF(io)

  private def doShowValidationReport(opts:MainOpts, result: Result): ESIO[Unit] = 
    if (opts.showValidationReport()) 
      for {
        rdf <- fromEither(result.validationReport)
        str <- fromIO(rdf.serialize(opts.validationReportFormat()))
        _ <- fromUnit(println(str))
      } yield (())
    else done

  private def printTime(msg: String, opts: MainOpts, nanos: Long): IO[Unit] = {
    if (opts.time()) {
      val time = Duration(nanos, NANOSECONDS).toMillis
      IO(println(f"$msg%s, $time%10d"))
    } else IO(())
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

  private def getShapeMapStr(opts: MainOpts, baseFolder: Path): EitherT[IO, String, String] = {
    if (opts.shapeMap.isDefined) {
      // val shapeMapFormat = opts.shapeMapFormat.toOption.getOrElse("COMPACT")
      val path = baseFolder.resolve(opts.shapeMap())
      for {
        // TODO: Allow different shapeMap formats
        content <- FileUtils.getContents(path.toFile)
      } yield content.toString
    } else EitherT.pure[IO,String]("")
  }

  private def getNodeSelector(opts:MainOpts, pm: PrefixMap): EitherT[IO, String, NodeSelector] = {
    if (opts.shapeInferNode.isDefined) {
      EitherT.fromEither[IO](NodeSelector.fromString(opts.shapeInferNode(),None,pm))
    } else
      EitherT.leftT[IO, NodeSelector](s"shapeInfer option requires also shapeInferNode to specify a node selector")
  }

  private def ok[A](x:A):ESIO[A] = EitherT.pure(x)

  private def getRDFReader(opts: MainOpts, baseFolder: Path): EitherT[IO, String, RDFReader] = {
    if (opts.data.isDefined || opts.dataUrl.isDefined) {
      for {
        rdf <- if (opts.data.isDefined) {
          val path = baseFolder.resolve(opts.data())
          fromIO(RDFAsJenaModel.fromFile(path.toFile(), opts.dataFormat(), relativeBase))
        } else {
          fromIO(RDFAsJenaModel.fromURI(opts.dataUrl(), opts.dataFormat(), relativeBase))
        }
        newRdf <- if (opts.inference.isDefined) fromIO(rdf.applyInference(opts.inference()))
                  else ok(rdf) 
      } yield newRdf 
    } else if (opts.endpoint.isDefined) {
      fromIO(Endpoint.fromString(opts.endpoint()))
    } else {
      logger.info("RDF Data option not specified")
      fromIO(RDFAsJenaModel.empty)
    }
  }

  private def getSchema(opts: MainOpts, baseFolder: Path, rdf: RDFReader): EitherT[IO, String, Schema] = {
    if (opts.schema.isDefined) {
      val path = baseFolder.resolve(opts.schema())
      Schemas.fromFile(path.toFile(), opts.schemaFormat(), opts.engine(), relativeBaseStr)
    } else if (opts.schemaUrl.isDefined) {
      val str = Source.fromURL(opts.schemaUrl()).mkString
      Schemas.fromString(str,opts.schemaFormat(),opts.engine(),relativeBaseStr)
    }
    else {
      logger.info("Schema not specified. Extracting schema from data")
      Schemas.fromRDF(rdf, opts.engine())
    }

  }

  private def shapeInfer(opts: MainOpts, baseFolder: Path): IO[Unit] = {

    val dataOptions: EitherT[IO,String,(RDFReader,Schema,String)] = for {
     rdf <- getRDFReader(opts, baseFolder)
     nodeSelector <- getNodeSelector(opts,rdf.getPrefixMap())
     shapeLabel = opts.shapeInferLabel()
     // _ <- { println(s"shapeLabel: $shapeLabel"); Right(()) }
     shapeLabelIri <- EitherT.fromEither[IO](IRI.fromString(shapeLabel,None))
     // _ <- { println(s"shapeLabelIri: $shapeLabelIri"); Right(()) }
     eitherResult <- fromIO(SchemaInfer.runInferSchema(rdf, nodeSelector, opts.shapeInferEngine(), shapeLabelIri))
     pair <- eitherResult.fold(s => err(s"Error obtaining data: $s"), pair => ok(pair))
     // rpair <- getPair(pair)
     (schema,resultMap) = pair
     str <- fromIO(schema.serialize(opts.shapeInferFormat()))
    } yield (rdf,schema,str)
    dataOptions.fold(e => println(s"shapeInfer: Error $e"), values => {
      val (rdf,schema,str) = values
      println(s"Shape infered: $str")
    })
  }

}

