package es.weso.shaclex

import org.rogach.scallop._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.ValidationTrigger
import es.weso.shapeMaps.ShapeMap

class MainOpts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  lazy val defaultEngine = engines.head
  lazy val engines = Schemas.availableSchemaNames.map(_.toUpperCase) // List("SHEX","SHACL")
  lazy val defaultDataFormat = "TURTLE"
  lazy val dataFormats = RDFAsJenaModel.availableFormats.map(_.toUpperCase).distinct
  lazy val schemaFormats = Schemas.availableFormats.map(_.toUpperCase).distinct
  lazy val defaultSchemaFormat = "TURTLE"
  lazy val defaultTrigger = ValidationTrigger.default.name
  lazy val triggerModes = ValidationTrigger.triggerValues.map(_._1.toUpperCase).distinct
  lazy val resultFormats = Result.availableResultFormats
  lazy val defaultResultFormat = Result.defaultResultFormat
  lazy val defaultShapeMapFormat = ShapeMap.defaultFormat
  lazy val shapeMapFormats = ShapeMap.formats

  banner("""| shaclex: SHACL/ShEx processor
            | Options:
            |""".stripMargin)

  footer("Enjoy!")

  val schema = opt[String](
    "schema",
    short = 's',
    default = None,
    descr = "schema file")

  val schemaFormat = opt[String](
    "schemaFormat",
    noshort = true,
    default = Some(defaultSchemaFormat),
    descr = s"Schema format. Default ($defaultDataFormat) Possible values: ${showLs(schemaFormats)}",
    validate = isMemberOf(schemaFormats))

  val data = opt[String](
    "data",
    default = None,
    descr = "Data file(s) to validate",
    short = 'd')

  val dataFormat = opt[String](
    "dataFormat",
    default = Some(defaultDataFormat),
    descr = s"Data format. Default ($defaultDataFormat). Possible values = ${showLs(dataFormats)}",
    validate = isMemberOf(dataFormats),
    noshort = true)

  val shapeMap = opt[String](
    "shapeMap",
    default = None,
    descr = s"Shape map",
    noshort = true)

  val shapeMapFormat = opt[String](
    "shapeMapFormat",
    default = Some(defaultShapeMapFormat),
    descr = s"Shape Map format. Default ($defaultShapeMapFormat). Possible values = ${showLs(shapeMapFormats)}",
    validate = isMemberOf(shapeMapFormats),
    noshort = true)

  val showValidationReport = toggle(
    name = "showValidationReport",
    prefix = "no-",
    default = Some(false),
    descrYes = "show SHACL validation report",
    descrNo = "don't show SHACL validation report",
    noshort = true
  )

  val engine = opt[String](
    "engine",
    default = Some(defaultEngine),
    descr = s"Engine. Default ($defaultEngine). Possible values: ${showLs(engines)}",
    validate = isMemberOf(engines))

  val trigger = opt[String](
    "trigger",
    default = Some(defaultTrigger),
    descr = s"Trigger mode. Default ($defaultTrigger). Possible values: ${showLs(triggerModes)}",
    validate = isMemberOf(triggerModes))

  val explain = toggle(
    "explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more extra info about validation process",
    descrNo = "don't show extra info",
    noshort = true
  )

  val showResult = toggle(
    "showResult",
    prefix = "no-",
    default = Some(true),
    descrYes = "show result of validation",
    descrNo = "don't show result",
    noshort = true)

  val showSchema = toggle(
    "showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema",
    noshort = true)

  val showData = toggle(
    "showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show data",
    descrNo = "don't show data",
    noshort = true)

  val showShapeMap = toggle(
    "showShapeMap",
    prefix = "no-",
    default = Some(false),
    descrYes = "show input shape map",
    descrNo = "don't input show map",
    noshort = true)

  val outputFile = opt[String](
    "outputFile",
    default = None,
    descr = "save report a file",
    short = 'f')

  val outSchemaFormat = opt[String](
    "outSchemaFormat",
    default = Some(defaultSchemaFormat),
    descr = "schema format to show",
    noshort = true)

  val outShapeMapFormat = opt[String](
    "outShapeMapFormat",
    default = Some(defaultShapeMapFormat),
    descr = "format of shape map to show",
    noshort = true)

  val showLog = toggle(
    "showLog",
    prefix = "no-",
    default = Some(false),
    descrYes = "show log info",
    descrNo = "don't show log info",
    noshort = true)

  val resultFormat = opt[String](
    "resultFormat",
    default = Some(defaultResultFormat),
    descr = "format to show result",
    noshort = true,
    validate = isMemberOf(resultFormats))

  val cnvEngine = opt[String](
    "cnvEngine",
    default = None,
    descr = "convert schema to schema in another engine",
    noshort = true,
    validate = isMemberOf(engines))

  val outDataFormat = opt[String](
    "outDataFormat",
    default = None,
    descr = "data format to show",
    noshort = true)

  val baseFolder = opt[String](
    "baseFolder",
    default = None,
    descr = "base folder",
    short = 'b')

  val node = opt[String](
    "node",
    short = 'n',
    default = None,
    required = false,
    descr = "Node to validate")

  val shapeLabel = opt[String](
    "shape",
    short = 'l',
    default = None,
    required = false,
    descr = "Label (IRI) of Constraint in Schema")

  val inference = opt[String](
    "inference",
    default = None,
    required = false,
    descr = "Apply some inference before. Available values: RDFS")

  val server = toggle(
    "server",
    prefix = "no-",
    default = Some(false),
    descrYes = "server mode",
    descrNo = "command line mode",
    noshort = true)

  val time = toggle(
    "time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't show time",
    short = 't')

  override protected def onError(e: Throwable) = onError(e, builder)

  // Some utils...
  def showLs(ls: List[String]): String =
    ls.mkString(",")

  def isMemberOf(ls: List[String])(x: String): Boolean =
    ls contains (x.toUpperCase)

}
