package es.weso.shaclex
import org.rogach.scallop._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.ValidationTrigger
import es.weso.shapeMaps.ShapeMap

class MainOpts(arguments: Array[String],
               onError: (Throwable, Scallop) => Nothing
              ) extends ScallopConf(arguments) {

  private lazy val defaultEngine = engines.head
  private lazy val engines = Schemas.availableSchemaNames.map(_.toUpperCase) ++ List("None")// List("SHEX","SHACL")
  private lazy val defaultDataFormat = "TURTLE"
  private lazy val dataFormats = RDFAsJenaModel.availableFormats.map(_.toUpperCase).distinct
  private lazy val schemaFormats = Schemas.availableFormats.map(_.toUpperCase).distinct
  private lazy val defaultSchemaFormat = "TURTLE"
  private lazy val defaultTrigger = ValidationTrigger.default.name
  private lazy val triggerModes = ValidationTrigger.triggerValues.map(_._1.toUpperCase).distinct
  private lazy val resultFormats = Result.availableResultFormats
  private lazy val defaultResultFormat = Result.defaultResultFormat
  private lazy val defaultShapeMapFormat = ShapeMap.defaultFormat
  private lazy val shapeMapFormats = ShapeMap.formats
  private lazy val defaultValidationReportFormat = "TURTLE"
  private lazy val validationReportFormats = RDFAsJenaModel.availableFormats.map(_.toUpperCase).distinct

  banner("""| shaclex: SHACL/ShEx processor
            | Options:
            |""".stripMargin)

  footer("Enjoy!")

   val schema: ScallopOption[String] = opt[String](
    "schema",
    short = 's',
    default = None,
    descr = "schema file")

  val schemaFormat: ScallopOption[String] = opt[String](
    "schemaFormat",
    noshort = true,
    default = Some(defaultSchemaFormat),
    descr = s"Schema format. Default ($defaultDataFormat) Possible values: ${showLs(schemaFormats)}",
    validate = isMemberOf(schemaFormats))

  val schemaUrl: ScallopOption[String] = opt[String](
    "schemaUrl",
    noshort = true,
    default = None,
    descr = "schema Url")

  val data: ScallopOption[String] = opt[String](
    "data",
    default = None,
    descr = "Data file(s) to validate",
    short = 'd')

  val dataUrl: ScallopOption[String] = opt[String](
    "dataUrl",
    default = None,
    descr = "Data URL to validate",
    noshort = true
  )

  val dataFormat: ScallopOption[String] = opt[String](
    "dataFormat",
    default = Some(defaultDataFormat),
    descr = s"Data format. Default ($defaultDataFormat). Possible values = ${showLs(dataFormats)}",
    validate = isMemberOf(dataFormats),
    noshort = true)

  val endpoint: ScallopOption[String] = opt[String](
    "endpoint",
    default = None,
    descr = "Endpoint",
    short = 'e')

  val shapeMap: ScallopOption[String] = opt[String](
    "shapeMap",
    default = None,
    descr = s"Shape map",
    noshort = true)

  val shapeMapFormat: ScallopOption[String] = opt[String](
    "shapeMapFormat",
    default = Some(defaultShapeMapFormat),
    descr = s"Shape Map format. Default ($defaultShapeMapFormat). Possible values = ${showLs(shapeMapFormats)}",
    validate = isMemberOf(shapeMapFormats),
    noshort = true)

  val showValidationReport: ScallopOption[Boolean] = toggle(
    name = "showValidationReport",
    prefix = "no-",
    default = Some(false),
    descrYes = "show SHACL validation report",
    descrNo = "don't show SHACL validation report",
    noshort = true
  )

  val validationReportFormat: ScallopOption[String] = opt[String](
    "validationReportFormat",
    default = Some(defaultValidationReportFormat),
    descr = s"Engine. Default ($defaultValidationReportFormat). Possible values: ${showLs(validationReportFormats)}",
    validate = isMemberOf(validationReportFormats))

  val engine: ScallopOption[String] = opt[String](
    "engine",
    default = Some(defaultEngine),
    descr = s"Engine. Default ($defaultEngine). Possible values: ${showLs(engines)}",
    validate = isMemberOf(engines))

  val trigger: ScallopOption[String] = opt[String](
    "trigger",
    default = Some(defaultTrigger),
    descr = s"Trigger mode. Default ($defaultTrigger). Possible values: ${showLs(triggerModes)}",
    validate = isMemberOf(triggerModes))

  val explain: ScallopOption[Boolean] = toggle(
    "explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more extra info about validation process",
    descrNo = "don't show extra info",
    noshort = true
  )

  val showResult: ScallopOption[Boolean] = toggle(
    "showResult",
    prefix = "no-",
    default = Some(true),
    descrYes = "show result of validation",
    descrNo = "don't show result",
    noshort = true)

  val showSchema: ScallopOption[Boolean] = toggle(
    "showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema",
    noshort = true)

  val showData: ScallopOption[Boolean] = toggle(
    "showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show data",
    descrNo = "don't show data",
    noshort = true)

  val showShapeMap: ScallopOption[Boolean] = toggle(
    "showShapeMap",
    prefix = "no-",
    default = Some(false),
    descrYes = "show input shape map",
    descrNo = "don't input show map",
    noshort = true)

  val showClingo: ScallopOption[Boolean] = toggle(
    "showClingo",
    prefix = "no-",
    default = Some(false),
    descrYes = "show validation as a Clingo file",
    descrNo = "don't show validation as a Clingo file",
    noshort = true)

  val clingoFile: ScallopOption[String] = opt[String](
    "clingoFile",
    default = None,
    descr = "save validation to Clingo file",
    noshort = true)

  val outputFile: ScallopOption[String] = opt[String](
    "outFile",
    default = None,
    descr = "save report to file",
    short = 'f')

  val outSchemaFormat: ScallopOption[String] = opt[String](
    "outSchemaFormat",
    default = Some(defaultSchemaFormat),
    descr = "schema format to show",
    noshort = true)

  val outSchemaFile: ScallopOption[String] = opt[String](
    "outSchemaFile",
    default = None,
    descr = "filename to save output schema",
    noshort = true)

  val outShapeMapFormat: ScallopOption[String] = opt[String](
    "outShapeMapFormat",
    default = Some(defaultShapeMapFormat),
    descr = "format of shape map to show",
    noshort = true)

  val showLog: ScallopOption[Boolean] = toggle(
    "showLog",
    prefix = "no-",
    default = Some(false),
    descrYes = "show log info",
    descrNo = "don't show log info",
    noshort = true)

  val resultFormat: ScallopOption[String] = opt[String](
    "resultFormat",
    default = Some(defaultResultFormat),
    descr = "format to show result",
    noshort = true,
    validate = isMemberOf(resultFormats))

  val outEngine: ScallopOption[String] = opt[String](
    "outEngine",
    default = None,
    descr = s"output schema engine (for conversions). Possible values: ${showLs(engines)}",
    noshort = true,
    validate = isMemberOf(engines))

  val outDataFormat: ScallopOption[String] = opt[String](
    "outDataFormat",
    default = None,
    descr = "data format to show",
    noshort = true)

  val baseFolder: ScallopOption[String] = opt[String](
    "baseFolder",
    default = None,
    descr = "base folder",
    short = 'b')

  val node: ScallopOption[String] = opt[String](
    "node",
    short = 'n',
    default = None,
    required = false,
    descr = "Node to validate")

  val shapeLabel: ScallopOption[String] = opt[String](
    "shape",
    short = 'l',
    default = None,
    required = false,
    descr = "Label (IRI) of Constraint.scala in Schema")

  val inference: ScallopOption[String] = opt[String](
    "inference",
    default = None,
    required = false,
    descr = "Apply some inference before. Available values: RDFS")

  val shapeInfer: ScallopOption[Boolean] = toggle("shapeInfer",
    prefix="no-",
    default = Some(false),
    descrYes = "infer Shape from Data",
    descrNo = "don't infer Shape from Data"
    )

  val shapeInferNode: ScallopOption[String] = opt[String]("shapeInferNode",
    default = None,
    required = false,
    descr = "Node selector for shapeInfer option"
  )

  val shapeInferLabel: ScallopOption[String] = opt[String]("shapeInferLabel",
    default = Some("Shape"),
    required = false,
    descr = "Shape label for shapeInfer option"
  )

  val shapeInferEngine: ScallopOption[String] = opt[String]("shapeInferEngine",
    default = Some("ShEx"),
    required = false,
    descr = "Engine for shapeInfer option"
  )

  val shapeInferFormat: ScallopOption[String] = opt[String]("shapeInferFormat",
    default = Some("ShExC"),
    required = false,
    descr = "Format for shapeInfer option"
  )

  val testShEx: ScallopOption[String] = opt[String]("testShEx",
    default = None,
    required = false,
    descr = "run a test from test-suite. ALL means run all tests"
  )

  val time: ScallopOption[Boolean] = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't show time",
    short = 't')

  override protected def onError(e: Throwable): Unit = onError(e, builder)

  // Some utils...
  private def showLs(ls: List[String]): String =
    ls.mkString(",")

  private def isMemberOf(ls: List[String])(x: String): Boolean =
    ls contains x.toUpperCase

}
