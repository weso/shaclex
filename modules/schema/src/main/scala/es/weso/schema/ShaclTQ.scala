package es.weso.schema

// import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect._
import cats.effect.concurrent._
import scala.util.control.NoStackTrace
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.ModelFactory
import java.io.StringReader
import org.apache.jena.riot._
import org.apache.jena.riot.Lang
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.graph.Graph
import org.apache.jena.riot.system.{PrefixMap => _, _}
import org.apache.jena.riot.RDFLanguages
import es.weso.shapeMaps.ResultShapeMap
import collection.JavaConverters._
import es.weso.shapeMaps.ShapeMap
import java.io._
import es.weso.utils.JenaUtils
import org.topbraid.shacl.validation.ValidationUtil
import org.topbraid.jenax.util.JenaDatatypes
import org.topbraid.shacl.vocabulary.SH
import org.apache.jena.vocabulary.RDF

case class ShaclTQException(msg: String) extends Exception(msg) with NoStackTrace


case class ShaclTQ(shapesGraph: Model) extends Schema {
  override def name = "SHACL_TQ"

  override def formats: Seq[String] = DataFormats.formatNames ++ Seq(Lang.SHACLC.getName().toUpperCase())

  override def defaultTriggerMode: ValidationTrigger = TargetDeclarations

  override def validate(rdf: RDFReader, trigger: ValidationTrigger, builder: RDFBuilder): IO[Result] = trigger match {
    case TargetDeclarations => validateTargetDecls(rdf).map(_.addTrigger(trigger))
    case _ => IO(Result.errStr(s"Not implemented trigger ${trigger.name} for ${name} yet"))
  }

  private def validateTargetDecls(rdf: RDFReader): IO[Result] = rdf match {
    case rdfJena: RDFAsJenaModel => for {
      rdfModel <- rdfJena.getModel
      pm <- rdfJena.getPrefixMap
      shapesPm = shapesGraph.getNsPrefixMap()
      report <- IO {
        val report: Resource = ValidationUtil.validateModel(rdfModel, shapesGraph, true);

        // val report: ValidationReport = ShaclValidator.get().validate(shapesGraph.getGraph(), rdfModel.getGraph())
        report
      } 
      result <- report2Result(report, pm, prefixMapFromModel(shapesGraph))
    } yield result
    case _ => IO.raiseError(ShaclTQException(s"Not Implemented Jena SHACL validation for ${rdf.rdfReaderName} yet"))
  }

  private def prefixMapFromModel(model: Model): PrefixMap = PrefixMap(model.getNsPrefixMap().asScala.toMap.map {
      case (alias, iri) => (Prefix(alias), IRI(iri))
  })

  private def report2Result(
    report: Resource, 
    nodesPrefixMap: PrefixMap, 
    shapesPrefixMap: PrefixMap
  ): IO[Result] = for {
    eitherRdf <- report2reader(report.getModel()).attempt
    isValid <- conforms(report)
    numViolations <- countViolations(report)
  } yield {  
    val message = if (isValid) s"Validated" 
     else s"Number of violations: ${numViolations}"
    val shapesMap = report2ShapesMap()
    val errors: Seq[ErrorInfo] = report2errors()
    // val esRdf = eitherRdf.leftMap(_.getMessage())
    Result(isValid = isValid, 
      message = message, 
      shapeMaps = Seq(shapesMap), 
      validationReport = JenaShaclReport(report.getModel), 
      errors = errors,
      trigger = Some(TargetDeclarations), 
      nodesPrefixMap = nodesPrefixMap, 
      shapesPrefixMap = shapesPrefixMap)
  }

  private def conforms(report: Resource): IO[Boolean] = 
    IO { report.hasProperty(SH.conforms,JenaDatatypes.TRUE) }

  private def countViolations(report: Resource): IO[Int] = 
    IO { report.getModel.listResourcesWithProperty(RDF.`type`,SH.ValidationResult).toList.size }


  private def report2reader(model: Model): IO[RDFReader] = for {
    refModel <- Ref.of[IO, Model](model)
  } yield RDFAsJenaModel(refModel,None,None)
    

  private def report2errors(): Seq[ErrorInfo] = Seq()

  private def report2ShapesMap(): ResultShapeMap = {
    ResultShapeMap.empty
  }

  override def fromString(cs: CharSequence, 
                          format: String, 
                          base: Option[String]
                          ): IO[Schema] = for {
    model <- IO { 
      val m : Model       = ModelFactory.createDefaultModel() 
      val str_reader      = new StringReader(cs.toString)
      val g: Graph        = m.getGraph
      val dest: StreamRDF = StreamRDFLib.graph(g)
      RDFParser.create.source(str_reader).lang(RDFLanguages.shortnameToLang(format)).parse(dest)
      m
    }
  } yield ShaclTQ(model)

  // private def err[A](msg:String): EitherT[IO,String, A] = EitherT.leftT[IO,A](msg)

  override def fromRDF(rdf: RDFReader): IO[es.weso.schema.Schema] = rdf match {
    case rdfJena: RDFAsJenaModel => for {
      _ <- IO { println(s"SHACL_TQ: Parsing Shapes graph from RDF data")}
      model <- rdfJena.getModel
      str <- rdfJena.serialize("TURTLE")
      _ <- IO { println(s"RDF to parse:\n${str}")}
    } yield ShaclTQ(model)
    case _ => IO.raiseError(ShaclTQException(s"Cannot obtain ${name} from RDFReader ${rdf.rdfReaderName} yet"))
  }

  override def serialize(format: String, base: Option[IRI]): IO[String] = 
    if (formats.contains(format.toUpperCase)) IO  {
      val out              = new ByteArrayOutputStream()
      val relativizedModel = JenaUtils.relativizeModel(shapesGraph, base.map(_.uri))
      relativizedModel.write(out, format)
      out.toString
    }
    else 
      IO.raiseError(ShaclTQException(s"Format $format not supported to serialize $name. Supported formats=$formats"))

  override def empty: Schema = ShaclTQ.empty

  override def shapes: List[String] = {
    List()
  }

  override def pm: PrefixMap = prefixMapFromModel(shapesGraph)

  override def convert(targetFormat: Option[String],
                       targetEngine: Option[String],
                       base: Option[IRI]
                      ): IO[String] = {
   targetEngine.map(_.toUpperCase) match {
     case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some("SHACL") | Some("SHACLEX") =>
       serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some("SHEX") => 
       IO.raiseError(ShaclTQException(s"Not implemented conversion between ${name} to ShEx yet")) 
     case Some(other) =>
       IO.raiseError(ShaclTQException(s"Conversion $name -> $other not implemented yet"))
   }
  }

  override def info: SchemaInfo = {
    // TODO: Check if shacl schemas are well formed
    SchemaInfo(name,"SHACLex", isWellFormed = true, List())
  }

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String] =
    IO.raiseError(ShaclTQException(s"Not implemented yet toClingo for $name"))

}

object ShaclTQ {
  def empty: ShaclTQ = {
    val m = ModelFactory.createDefaultModel()
    ShaclTQ(m)
  }

}
