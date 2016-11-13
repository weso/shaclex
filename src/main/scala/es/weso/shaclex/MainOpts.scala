package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.ValidationTrigger


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

  banner("""| shaclex: SHACL processor
            | Options:
            |""".stripMargin)

  footer("Enjoy!")

  val schema = opt[String]("schema",
    short = 's',
    default = None,
    descr = "schema file"
  )

  val schemaFormat = opt[String]("schemaFormat",
    noshort = true,
    default = Some(defaultSchemaFormat),
    descr = s"Schema format. Default ($defaultDataFormat) Possible values: ${showLs(schemaFormats)}",
    validate = isMemberOf(schemaFormats)
  )

  val data = opt[String]("data",
    default = None,
    descr = "Data file(s) to validate",
    short = 'd'
  )

  val dataFormat = opt[String]("dataFormat",
    default = Some(defaultDataFormat),
    descr = s"Data format. Default ($defaultDataFormat). Possible values = ${showLs(dataFormats)}",
    validate = isMemberOf(dataFormats),
    noshort = true
  )

  val engine = opt[String]("engine",
    default = Some(defaultEngine),
    descr = s"Engine. Default ($defaultEngine). Possible values: ${showLs(engines)}",
    validate = isMemberOf(engines)
  )

  val trigger = opt[String]("trigger",
    default = Some(defaultTrigger),
    descr = s"Trigger mode. Default ($defaultTrigger). Possible values: ${showLs(triggerModes)}",
    validate = isMemberOf(triggerModes)
  )

  val explain = toggle("explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more extra info about validation process",
    descrNo = "don't show extra info",
    noshort = true
    )

  val showResult = toggle("showResult",
    prefix = "no-",
    default = Some(true),
    descrYes = "show result of validation",
    descrNo = "don't show result",
    noshort = true
  )

  val showSchema = toggle("showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema",
    noshort = true
    )

  val showData = toggle("showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show data",
    descrNo = "don't show data",
    noshort = true
  )

  val outputFile = opt[String]("outputFile",
    default = None,
    descr = "save report a file",
    short = 'f'
    )

  val outSchemaFormat = opt[String]("outSchemaFormat",
    default = None,
    descr = "schema format to show",
    noshort = true
  )

  val outResultFormat = opt[String]("outResultFormat",
    default = Some(defaultResultFormat),
    descr = "format to show result",
    noshort = true,
    validate = isMemberOf(resultFormats)
  )

  val cnvEngine = opt[String]("cnvEngine",
    default = None,
    descr = "convert schema to schema in another engine",
    noshort = true,
    validate = isMemberOf(engines)
  )

  val outDataFormat = opt[String]("outDataFormat",
    default = None,
    descr = "data format to show",
    noshort = true
  )

  val node = opt[String]("node",
    short = 'n',
    default = None,
    required = false,
    descr = "Node to validate")

  val shapeLabel = opt[String]("shape",
    short = 'l',
    default = None,
    required = false,
    descr = "Label (IRI) of Shape in Schema")

  val time = toggle("time",
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
