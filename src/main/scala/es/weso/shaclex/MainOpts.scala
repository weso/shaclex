package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
// import es.weso.shacl._
import es.weso.rdf.jena.RDFAsJenaModel


class MainOpts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  lazy val engines = List("SHEX","SHACL")
  lazy val defaultDataFormat = "TURTLE"
  lazy val dataFormats = RDFAsJenaModel.availableFormats
  lazy val schemaFormats = dataFormats ++ List("ShExC")


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
    default = Some(defaultDataFormat),
    descr = "schema format"
  )

  val data = opt[String]("data",
    default = None,
    descr = "Data file(s) to validate",
    short = 'd'
  )

  val dataFormat = opt[String]("dataFormat",
    default = Some(defaultDataFormat),
    descr = "Data format",
    noshort = true
  )

  val engine = opt[String]("engine",
    default = Some("SHACL"),
    descr = s"Engine. Possible values: ${engines.mkString(",")}",
    validate = x => engines contains (x.toUpperCase())
  )

  val explain = toggle("explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more extra info about validation process",
    descrNo = "don't show extra info",
    noshort = true
    )

  val show = toggle("show",
    prefix = "no-",
    default = Some(false),
    descrYes = "show report",
    descrNo = "don't show report",
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
