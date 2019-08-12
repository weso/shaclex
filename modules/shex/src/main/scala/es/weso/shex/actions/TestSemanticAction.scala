package es.weso.shex.actions
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, RDFNode}

object TestSemanticAction {
  val iri = IRI("http://shex.io/extensions/Test/")

  def runAction(code: String, node: RDFNode, rdf: RDFReader): Either[String,Unit] = {
    val printExpr = "print\\((.*)\\)".r
    val failExpr = "fail\\((.*)\\)".r
    val sExpr = raw"s".r
    val cleanedStr = code.stripPrefix(" ").stripSuffix(" ")
    cleanedStr match {
      case printExpr(str) => {
        str match {
          case sExpr() => println(s"$node")
          case _ => println(str)
        }
        Right(())
      }
      case failExpr(str) => {
        val s = str match {
          case sExpr() => s"$node"
          case _ => str
        }
        println(s)
        Left(s"TestSemanticAction: Fail expression: $s")
      }
      case str => {
        println(s"TestSemanticAction: Does not match: $str")
        Right(())
      }
    }
  }
}