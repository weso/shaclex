package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.schema._
import es.weso.rdf.jena.RDFAsJenaModel


class MainOpts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  lazy val defaultEngine = engines.head
  lazy val engines = Schemas.availableSchemaNames.map(_.toUpperCase) // List("SHEX","SHACL")
  lazy val defaultDataFormat = "TURTLE"
  lazy val dataFormats = RDFAsJenaModel.availableFormats.map(_.toUpperCase).distinct
  lazy val schemaFormats = Schemas.availableFormats.map(_.toUpperCase).distinct
  lazy val defaultSchemaFormat = "TURTLE"

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

  def showLs(ls: List[String]): String =
    ls.mkString(",")

  def isMemberOf(ls: List[String])(x: String): Boolean =
    ls contains (x.toUpperCase)

  val engine = opt[String]("engine",
    default = Some(defaultEngine),
    descr = s"Engine. Default ($defaultEngine). Possible values: ${showLs(engines)}",
    validate = isMemberOf(engines)
  )

  val explain = toggle("explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more extra info about validation process",
    descrNo = "don't show extra info",
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

  val time = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't show time",
    short = 't')

  override protected def onError(e: Throwable) = onError(e, builder)

}
