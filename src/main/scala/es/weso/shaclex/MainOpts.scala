package es.weso.wiGen

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shacl._
import es.weso.main.Processors

class MainOpts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| wiGen: WebIndex-like data generation and validation
            | Options:
            |""".stripMargin)

  footer("Enjoy!")

  val numCountries = opt[Int]("countries",
    short = 'c',
    default = Some(1),
    descr = "Number of countries"
  )

  val numDataSets = opt[Int]("dataSets",
    short = 'd',
    default = Some(1),
    descr = "Number of data sets"
  )
    
  val numSlices = opt[Int]("slices",
    short = 'l',
    default = Some(1),
    descr = "Number of slices"
  )
  
  val numObs = opt[Int]("observations",
    short = 'o',
    default = Some(1),
    descr = "Number of observations"
  )
    
  val numComps = opt[Int]("computations",
    short = 'p',
    default = Some(1),
    descr = "Number of computations"
  )
  
  val numIndicators = opt[Int]("Indicators",
    short = 'i',
    default = Some(1),
    descr = "Number of indicators"
  )

  val numOrgs = opt[Int]("Organizations",
    short = 'g',
    default = Some(1),
    descr = "Number of organizations"
  )
  
  val numBadCountries = opt[Int]("badCountries",
    default = Some(0),
    descr = "Number of invalid countries",
    noshort = true
  )

  val numBadDataSets = opt[Int]("badDataSets",
    default = Some(0),
    descr = "Number of invalid dataSets",
    noshort = true
  )
  
  val numBadSlices = opt[Int]("badSlices",
    default = Some(0),
    descr = "Number of invalid slices",
    noshort = true
  )
  
  val numBadObs = opt[Int]("badObs",
    default = Some(0),
    descr = "Number of invalid observations",
    noshort = true
  )
  
  val numBadComps = opt[Int]("badComps",
    default = Some(0),
    descr = "Number of invalid computations",
    noshort = true
  )
  
  val numBadIndicators = opt[Int]("badIndicators",
    default = Some(0),
    descr = "Number of invalid indicators",
    noshort = true
  )
  
  val numBadOrgs = opt[Int]("badOrgs",
    default = Some(0),
    descr = "Number of invalid organizations",
    noshort = true
  )
  
  val allTypes = toggle("allTypes",
    prefix = "no-",
    default = Some(false),
    descrYes = "add rdf:type to every node",
    descrNo = "don't add rdf:type to every node",
    noshort = true
    )
    
  val explain = toggle("explain",
    prefix = "no-",
    default = Some(false),
    descrYes = "show more info in case of errors",
    descrNo = "don't show info in case of errors",
    noshort = true
    )
    
  val show = toggle("show",
    prefix = "no-",
    default = Some(false),
    descrYes = "show data generated",
    descrNo = "don't show data generated",
    noshort = true
    )
    
  val format = opt[String]("format",
    default = Some("TURTLE"),
    descr = "format"
    )
    
  val outputFile = opt[String]("file",
    default = None,
    descr = "save generated data in a file",
    short = 'f'
    )
    
  val allScopeNodes = toggle("scopeNodes",
    prefix = "no-",
    default = Some(true),
    descrYes = "generate all scopeNode declarations",
    descrNo = "generate only one scopeNode declaration of a dataSet",
    noshort = true)
    
  val shex = opt[String]("shex",
    default = None,
    descr = "Validate with ShEx schema",
    short = 'x')
    
  val shacl = opt[String]("shacl",
    default = None,
    descr = "Validate with SHACL schema",
    short = 's')
    
  val time = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't show time",
    short = 't')
    
  override protected def onError(e: Throwable) = onError(e, builder)

}
