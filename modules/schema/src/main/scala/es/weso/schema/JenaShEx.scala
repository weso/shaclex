package es.weso.schema

// import cats.implicits._
import cats.effect._
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shapemaps.ShapeMap
import org.apache.jena.riot.system.{PrefixMap => _}
import org.apache.jena.shex._
import scala.util.control.NoStackTrace
import collection.JavaConverters._
import es.weso.shapemaps.RDFNodeSelector
import es.weso.shapemaps.ShapeMapLabel
import org.apache.jena.graph.Node
import org.apache.jena.graph.NodeFactory
import es.weso.shapemaps.IRILabel
import cats.implicits._


case class JenaShExException(msg: String) extends Exception(msg) with NoStackTrace


case class JenaShEx(schema: ShexSchema) extends Schema {
  override def name = "JenaShEx"

  override def formats: Seq[String] = List("ShExC") // Does Jena-ShEx support JSON-LD ?

  override def defaultTriggerMode: ValidationTrigger = ShapeMapTrigger.empty

  override def validate(rdf: RDFReader, trigger: ValidationTrigger, builder: RDFBuilder): IO[Result] = trigger match {
    case sm : ShapeMapTrigger => rdf match {
      case rdfJena: RDFAsJenaModel => validateJenaShEx(rdfJena,sm,builder)
      case _ => IO(Result.errStr(s"Not implemented JenaShex for non-RDFAsJenaModel ${rdf.getClass().getCanonicalName()} yet"))
    }
    case _ => IO(Result.errStr(s"Not implemented trigger ${trigger.name} for ${name} yet"))
  }

  private def validateJenaShEx(rdf: RDFAsJenaModel, sm: ShapeMapTrigger, builder: RDFBuilder): IO[Result] = 

    for {
      shapeMap <- convertShapeMap(sm)
      model <- rdf.modelRef.get
      graph = model.getGraph
      validator = ShexValidator.get()
      report <- IO.blocking(validator.validate(graph,schema,shapeMap))
      result <- report2Result(report)
    } yield result

  private def convertShapeMap(sm: ShapeMapTrigger): IO[ShexMap] = {
    val builder = sm.shapeMap.associations.foldRight(IO(ShexMap.newBuilder())) {
      case (a,b) => a.node match {
        case RDFNodeSelector(node) => for {
          n <- cnvNode(node)
          lbl <- cnvLabel(a.shape)
          builder <- b
        } yield builder.add(n, lbl)
        case _ => IO.raiseError(JenaShExException(s"convertShapeMap: not implemented node selectors: ${a.node}"))
      }
    }
    builder.map(_.build())
  }

  private def cnvNode(node: RDFNode): IO[Node] = node match {
    case i: IRI => IO.pure(NodeFactory.createURI(i.str))
    case _ => IO.raiseError(JenaShExException(s"cnvNode: unsupported $node, must be an IRI"))
  }

  private def cnvLabel(label: ShapeMapLabel): IO[Node] = label match {
    case IRILabel(i) => IO.pure(NodeFactory.createURI(i.str))
    case _ => IO.raiseError(JenaShExException(s"cnvLabel: unsupported $label, must be an IRI"))
  }
  
  private def report2Result(report: ShexReport): IO[Result] = { 
   val message = if (report.conforms()) s"Validated" 
     else s"Not valid"
   Result(isValid = report.conforms(), message,
      shapeMaps = Seq(),
      validationReport = RDFReport.empty,
      errors = Seq(),
      None,
      PrefixMap.empty,
      PrefixMap.empty).pure[IO]
  }

  override def fromString(str: String, 
                          format: String, 
                          base: Option[String]
                          ): IO[Schema] = for {
    schema <- IO { 
     Shex.schemaFromString(str) 
    }
  } yield JenaShEx(schema)

  // private def err[A](msg:String): EitherT[IO,String, A] = EitherT.leftT[IO,A](msg)

  override def fromRDF(rdf: RDFReader): IO[es.weso.schema.Schema] = rdf match {
    case _ => IO.raiseError(JenaShaclException(s"Cannot obtain ${name} from RDFReader ${rdf.rdfReaderName} yet"))
  }

  override def serialize(format: String, base: Option[IRI]): IO[String] = 
    if (formats.contains(format.toUpperCase)) IO {
      // TODO: Experimenta...check if we can leverage Jena to print the schema...
      val out = new StringBuilder()
      schema.getShapes().forEach{ se => out ++= se.toString }
      out.toString
    }
    else 
      IO.raiseError(JenaShExException(s"Format $format not supported to serialize $name. Supported formats=$formats"))

  override def empty: Schema = JenaShEx.empty

  override def shapes: List[String] = {
    schema.getShapes().asScala.toList.map(s => s.getLabel().toString)
  }

  override def pm: PrefixMap = { 
    val pms: Map[String,IRI] = schema.getPrefixMap().getMapping.asScala.toMap.map {
    case (alias, iriStr) => (alias, IRI(iriStr))
    }
    PrefixMap.fromMap(pms)
  } 

  override def convert(targetFormat: Option[String],
                       targetEngine: Option[String],
                       base: Option[IRI]
                      ): IO[String] = {
   targetEngine.map(_.toUpperCase) match {
     case None => serialize(targetFormat.getOrElse(DataFormats.defaultFormatName))
     case Some(other) =>
       IO.raiseError(JenaShaclException(s"Conversion $name -> $other not implemented yet"))
   }
  }

  override def info: SchemaInfo = {
    // TODO: Check if shacl schemas are well formed
    SchemaInfo(schema.getSource(), name, isWellFormed = true, List())
  }

  override def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String] =
    IO.raiseError(JenaShaclException(s"Not implemented yet toClingo for $name"))

}

object JenaShEx {

  def empty: JenaShEx = {
    JenaShEx(Shex.schemaFromString(""))
  }

}
