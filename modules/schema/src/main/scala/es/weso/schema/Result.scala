package es.weso.schema
import es.weso.rdf.PrefixMap

case class Result(
    isValid: Boolean,
    message: String,
    solutions: Seq[Solution],
    errors: Seq[ErrorInfo]) {

  def noSolutions(sols: Seq[Solution]): Boolean = {
    sols.size == 1 && sols.head.isEmpty
  }


/*  def toHTML(cut: Int = 1, schema:Schema): String = {
    val sb = new StringBuilder
    val pm = schema.pm
    if (isValid) {
      if (noSolutions(solutions)) {
        sb ++= "<h2>No solutions found</h2"
      } else {
     for ((solution, n) <- solutions zip (1 to cut)) {
      sb ++= "<h2 class='result'>Result" + printNumber(n, cut) + "</h2>"
      sb ++= schema.htmlBeforeSolutions
      sb ++= solution.toHTML(pm)
      sb ++= schema.htmlAfterSolutions
     }
     }
    } else {
    val numErrors = errors.size
    val errorStr = if (numErrors == 1) "Error" else "Errors"
    sb ++="<div class=\"errors\">"
    sb ++= s"<p class='errorMsg'>${numErrors} $errorStr found</p>"
    sb ++= "<table class='display' id='results' >"
    sb ++= schema.htmlBeforeErrors
    for (error <- errors) {
      sb ++= error.toHTML(pm)
     }
    sb ++= schema.htmlAfterErrors
    sb++="</table>"
    }
    sb.toString
  }

  }
 */

  lazy val cut = 1 // TODO maybe remove concept of cut

  def printNumber(n: Int, cut: Int): String = {
    if (n == 1 && cut == 1) ""
    else n.toString
  }
  
  def show(pm: PrefixMap): String = {
    val sb = new StringBuilder
    if (isValid) {
      if (noSolutions(solutions)) {
        "No solutions found"
      } else {
     for ((solution, n) <- solutions zip (1 to cut)) {
      sb ++= "Result " + printNumber(n, cut)
      sb ++= solution.show(pm)
     }
     }
    }
    else
      sb ++= errors.map(_.show(pm)).mkString("\n")
    sb.toString
 }

}

object Result {
  def empty =
    Result(isValid = true,
           message = "",
           solutions = Seq(),
           errors=Seq())

  def errStr(str: String) = Result(isValid = false, message = str, solutions = Seq(), errors = Seq())

}
