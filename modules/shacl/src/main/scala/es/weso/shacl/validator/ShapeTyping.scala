package es.weso.shacl.validator
import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode
import es.weso.shacl.Shape
import es.weso.shacl.report._
import es.weso.typing._

case class ShapeTyping(t: Typing[RDFNode, Shape, AbstractResult, String]) {

  def getNodes: Seq[RDFNode] = t.getKeys

  def getMap : Map[RDFNode, Map[Shape, TypingResult[AbstractResult, String]]] =
    t.getMap

  def hasType(node: RDFNode, shape: Shape): Boolean =
    t.hasType(node,shape)

  def addType(node: RDFNode, shape: Shape): ShapeTyping =
    ShapeTyping(t.addType(node,shape))

  def addEvidence(node: RDFNode, shape: Shape, msg: String): ShapeTyping =
    ShapeTyping(t.addEvidence(node, shape, msg))

  def addNotEvidence(node: RDFNode, shape: Shape, e: AbstractResult): ShapeTyping =
    ShapeTyping(t.addNotEvidence(node,shape,e))

  def getFailedValues(node: RDFNode): Set[Shape] =
    t.getFailedValues(node)

  def getOkValues(node: RDFNode): Set[Shape] =
    t.getOkValues(node)

  def toValidationReport: ValidationReport = {
    ValidationReport(
      conforms = t.allOk,
      results = {
        val rs: Seq[(RDFNode, Shape, TypingResult[AbstractResult, String])] =
          t.getMap.toSeq.map {
            case (node,valueMap) => valueMap.toSeq.map {
              case (shape, result) => (node, shape, result)
            }
          }.flatten
        rs.map(_._3.getErrors.toList.flatten).flatten
      },
      shapesGraphWellFormed = true
    )
  }

  override def toString: String = Show[ShapeTyping].show(this)

}

object ShapeTyping {
  def empty: ShapeTyping = ShapeTyping(Typing.empty)

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping =
    ShapeTyping(Typing.combineTypings(ts.map(_.t)))

  implicit def showShapeTyping: Show[ShapeTyping] = new Show[ShapeTyping] {
    override def show(st: ShapeTyping): String = {
      val sb: StringBuilder = new StringBuilder
      st.getMap.toList.map{ case (node,shapeMap) => {
         shapeMap.toList.map{ case (shape, typingResult) => {
           sb.append(s"${node.show}-${shape.showId} = ${showTypingResult(typingResult)}\n")
         }}
        }
      }
      sb.toString
    }

    private def showTypingResult(tr: TypingResult[AbstractResult, String]): String =
      if (tr.isOK) "Valid"
      else "Not valid"
  }

  implicit def monoidShapeTyping: Monoid[ShapeTyping] = {
    new Monoid[ShapeTyping] {
      override def empty: ShapeTyping = ShapeTyping.empty

      override def combine(t1: ShapeTyping, t2: ShapeTyping): ShapeTyping =
        ShapeTyping(Typing.combineTypings(Seq(t1.t, t2.t)))
    }
  }

}

