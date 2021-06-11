package es.weso.shaclex

import cats.implicits._
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.scalalogging._
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.shaclex.repl.Repl

import scala.io.Source
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import scala.concurrent.duration._
import es.weso.utils.FileUtils
import scala.util._
import java.nio.file._
import es.weso.rdf.RDFReader
//import cats.data.EitherT
import cats.effect._
import cats._
// import scala.concurrent.ExecutionContext
import es.weso.rdf.RDFBuilder
import es.weso.rdf.InferenceEngine
// import cats.implicits._

object Main extends IOApp with LazyLogging {

  type ESIO[A] = IO[A] // EitherT[IO, String, A]

  private def fromUnit(x: => Unit): ESIO[Unit] = 
    // EitherT.liftF[IO,String,Unit](IO(x))
    IO(x)

  private def fromEither[A](e: Either[String,A]): ESIO[A] =
    // EitherT.fromEither[IO](e)
    IO.fromEither(e.leftMap(s => new RuntimeException(s)))

  private def fromIO[A](io: IO[A]): ESIO[A] = io
  private def done: IO[Unit] = ().pure[IO]
  //private def err[A](msg: String): ESIO[A] = fromIO(IO.raiseError(new RuntimeException(msg)))


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
    IO(()) //shapeInfer(opts,baseFolder)
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

    if (args.isEmpty) for {
      _ <- IO { opts.printHelp() }
    } yield ExitCode.Error
    else for {
     _ <- doShExTest(opts) 
     baseFolder <- getBaseFolder(opts) 
     startTime <- IO(System.nanoTime())
     _ <- doShapeInfer(baseFolder,opts)
     either <- doProcess(baseFolder, opts)
     code <- either.fold(
       err => IO(println(s"Error: $err")) >> IO.pure(ExitCode.Error),
       _ => IO.pure(ExitCode.Success)
     )
     _ <- doShowTime(startTime, opts)
     _ <- if (opts.shell()) {
       IO {
         new Repl(opts).runUntilQuit()
       }
     } else {
       IO.pure(())
     }
    } yield code
  }

  private def doProcess(baseFolder: Path, opts:MainOpts): IO[Either[Throwable,Unit]] = {
    val e: IO[Unit] = for {
      res1 <- getRDFReader(opts, baseFolder)
      res2 <- RDFAsJenaModel.empty
      vv <- (res1, res2).tupled.use { case (rdf,builder) => for {
    schema <- getSchema(opts, baseFolder, rdf)
    triggerName = opts.trigger.toOption.getOrElse(ValidationTrigger.default.name)
    shapeMapStr <- getShapeMapStr(opts, baseFolder)
    // _ <- IO { pprint.log(shapeMapStr) }
    pm <- rdf.getPrefixMap
    trigger <- fromEither(ValidationTrigger.findTrigger(triggerName, shapeMapStr, relativeBaseStr,
        opts.node.toOption, opts.shapeLabel.toOption,
        pm, schema.pm))
    _ <- doShowData(opts,rdf)    
    _ <- doShowSchema(opts,schema)
    _ <- doShowShapeMap(opts, trigger)
    _ <- doShowClingo(opts,rdf,schema,trigger)
    _ <- whenA(opts.validate(), doValidation(opts, rdf, schema, trigger,builder))
    } yield ()
   }
    } yield vv 
   e.attempt
  }

  private def doValidation(opts: MainOpts, 
      rdf: RDFReader, 
      schema: Schema, 
      trigger: ValidationTrigger, 
      builder: RDFBuilder): ESIO[Unit] = for {
    result <- schema.validate(rdf, trigger, builder)
    _ <- doShowResult(opts,result)
    _ <- doShowValidationReport(opts,result)
  } yield ()

  private def doShowData(opts:MainOpts, rdf:RDFReader): IO[Unit] = {
    if (opts.showData()) {
      // If not specified uses the input schema format
      val outDataFormat = opts.outDataFormat.getOrElse(opts.dataFormat())
      for { 
        str <- rdf.serialize(outDataFormat, relativeBase)
        _ <- IO(println(str))
      } yield ()
    } else {
      ().pure[IO]
    }
  }

  private def doShowSchema(opts:MainOpts, schema:Schema): IO[Unit] = {
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

  

  private def doShowShapeMap(opts:MainOpts, trigger: ValidationTrigger): ESIO[Unit] = 
   whenA(opts.showShapeMap(), 
     fromUnit(println(s"ShapeMap: ${trigger.shapeMap.serialize(opts.outShapeMapFormat(),relativeBase)}"))
   )

  private def doShowClingo(opts: MainOpts, rdf: RDFReader, schema: Schema, trigger: ValidationTrigger): ESIO[Unit] = 
    if (opts.clingoFile.isDefined || opts.showClingo()) for {
      str <- schema.toClingo(rdf, trigger.shapeMap) // .leftMap(e => s"Error converting to clingo: $e")
      _ <- whenA(opts.showClingo(),fromUnit(println(s"$str")))
      _ <- whenA(opts.clingoFile.isDefined, fromUnit(FileUtils.writeFile(opts.clingoFile(), str)))
    } yield (())
    else done

  
  private def doShowResult(opts: MainOpts, result: Result): ESIO[Unit] = 
  if (opts.showResult() || opts.outputFile.isDefined) {
    // val resultSerialized = result.serialize(opts.resultFormat(),relativeBase)
    for {
      resEmpty <-fromIO(RDFAsJenaModel.empty)
      str <- fromIO(resEmpty.use(builder => result.serialize(opts.resultFormat(),relativeBase,builder)))
      _ <- whenA(opts.showResult(),fromUnit(println(str)))
      _ <- whenA(opts.outputFile.isDefined,fromUnit(FileUtils.writeFile(opts.outputFile(), str)))
    } yield (())
  } else done


  private def doShowValidationReport(opts:MainOpts, result: Result): ESIO[Unit] = 
    if (opts.showValidationReport()) 
      for {
        res <- fromIO(RDFAsJenaModel.empty)
        str <- fromIO(res.use(builder => result.validationReport.toRDF(builder).flatMap(_.serialize(opts.validationReportFormat()))))
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

  private def getShapeMapStr(opts: MainOpts, baseFolder: Path): IO[String] = {
    if (opts.shapeMap.isDefined) {
      // val shapeMapFormat = opts.shapeMapFormat.toOption.getOrElse("COMPACT")
      val path = baseFolder.resolve(opts.shapeMap())
      for {
        // TODO: Allow different shapeMap formats
        content <- FileUtils.getContents(path)
      } yield content.toString
    } else "".pure[IO]
  }

/*  private def getNodeSelector(opts:MainOpts, pm: PrefixMap): EitherT[IO, String, NodeSelector] = {
    if (opts.shapeInferNode.isDefined) {
      EitherT.fromEither[IO](NodeSelector.fromString(opts.shapeInferNode(),None,pm))
    } else
      EitherT.leftT[IO, NodeSelector](s"shapeInfer option requires also shapeInferNode to specify a node selector")
  } */

  // private def ok[A](x:A):ESIO[A] = IO.pure(x)
  // private def okResource[A](x:A): Resource[IO,A] = Resource.pure[IO,A](x)

  private def getRDFReader(opts: MainOpts, baseFolder: Path): IO[Resource[IO,RDFReader]] = {
    if (opts.data.isDefined || opts.dataUrl.isDefined) {
      for {
        rdf <- if (opts.data.isDefined) {
          val path = baseFolder.resolve(opts.data())
          RDFAsJenaModel.fromFile(path.toFile(), opts.dataFormat(), relativeBase)
        } else {
          RDFAsJenaModel.fromURI(opts.dataUrl(), opts.dataFormat(), relativeBase)
        }
        newRdf = if (opts.inference.isDefined) for {
          inference <- Resource.eval(fromEither(InferenceEngine.fromString(opts.inference())))
          r <- rdf.evalMap(rdf => rdf.applyInference(inference))
        } yield r 
        else rdf
      } yield newRdf 
    } else if (opts.endpoint.isDefined) {
      IO(Resource.eval(Endpoint.fromString(opts.endpoint())))
    } else {
      logger.info("RDF Data option not specified")
      RDFAsJenaModel.empty
    }
  }

  private def getSchema(opts: MainOpts, baseFolder: Path, rdf: RDFReader): IO[Schema] = {
    val r: IO[Schema] = if (opts.schema.isDefined) {
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
   // val v: IO[Either[String,Schema]] = r.map(_.leftMap(_.getMessage))
   r
  }

/*  private def shapeInfer(opts: MainOpts, baseFolder: Path): IO[Unit] = {

    val dataOptions: IO[(RDFReader,Schema,String)] = getRDFReader(opts, baseFolder).use(rdf => for {
     pm <- EitherT.liftF(rdf.getPrefixMap)
     nodeSelector <- getNodeSelector(opts,pm)
     shapeLabel = opts.shapeInferLabel()
     // _ <- { println(s"shapeLabel: $shapeLabel"); Right(()) }
     shapeLabelIri <- EitherT.fromEither[IO](IRI.fromString(shapeLabel,None))
     // _ <- { println(s"shapeLabelIri: $shapeLabelIri"); Right(()) }
     eitherResult <- fromIO(SchemaInfer.runInferSchema(rdf, nodeSelector, opts.shapeInferEngine(), shapeLabelIri))
     pair <- eitherResult.fold(s => err(s"Error obtaining data: $s"), pair => ok(pair))
     // rpair <- getPair(pair)
     (schema,resultMap) = pair
     str <- fromIO(schema.serialize(opts.shapeInferFormat()))
    } yield (rdf,schema,str))

    /*dataOptions.fold(e => println(s"shapeInfer: Error $e"), values => {
      val (rdf,schema,str) = values
      println(s"Shape infered: $str")
    }) */
    dataOptions 
  } */

}

