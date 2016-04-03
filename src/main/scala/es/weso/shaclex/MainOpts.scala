package es.weso.shaclex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shacl._

class MainOpts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| shaclex: SHACL processor
            | Options:
            |""".stripMargin)

  footer("Enjoy!")
    
  val shacl = opt[String]("shacl",
    short = 's',
    default = None,
    descr = "shacl file"
  )
  
  val shaclFormat = opt[String]("shaclFormat",
    noshort = true,
    default = Some("TURTLE"),
    descr = "SHACL input format"
  )
  
  val data = opt[String]("data",
    default = None,
    descr = "Data file(s) to validate",
    short = 'd'
  )
  
  val dataFormat = opt[String]("dataFormat",
    default = Some("TURTLE"),
    descr = "Data format",
    noshort = true
  )
 
  val engine = opt[String]("engine",
    default = Some("SHACL_WD_16_01"),
    descr = "Data format",
    noshort = true
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
