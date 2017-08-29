package es.weso.shex.validator
import cats._
import data._
import cats.implicits._
import es.weso.typing._
import es.weso.rdf.nodes._
import es.weso.shex._

case class ShapeTyping(t: Typing[RDFNode, ShapeType, ViolationError, String]) {

  def getOkValues(node: RDFNode): Set[ShapeType] =
    t.getOkValues(node)

  def getFailedValues(node: RDFNode): Set[ShapeType] =
    t.getFailedValues(node)

  // TODO Review these definitions in case of anonymous shapes...
  def hasType(node: RDFNode, label: ShapeLabel): Boolean = {
    !getOkValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def hasNoType(node: RDFNode, label: ShapeLabel): Boolean = {
    !getFailedValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def addType(node: RDFNode, shapeType: ShapeType): ShapeTyping =
    this.copy(t = t.addType(node, shapeType))

  def addEvidence(node: RDFNode, shapeType: ShapeType, evidence: String): ShapeTyping =
    this.copy(t = t.addEvidence(node, shapeType, evidence))

  def addNotEvidence(node: RDFNode, shapeType: ShapeType, err: ViolationError): ShapeTyping =
    this.copy(t = t.addNotEvidence(node, shapeType, err))

  def getMap: Map[RDFNode, Map[ShapeType, TypingResult[ViolationError, String]]] =
    t.getMap

  override def toString = showShapeTyping

  def showShapeTyping: String = {
    import ShapeTyping._
    t.show
  }
}

object ShapeTyping {

  def emptyShapeTyping: ShapeTyping = {
    val emptyTyping: Typing[RDFNode, ShapeType, ViolationError, String] = Typing.empty
    ShapeTyping(emptyTyping)
  }

  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    def show(n: RDFNode) = s"$n"
  }

  implicit def showShapeTyping = new Show[ShapeTyping] {
    override def show(t: ShapeTyping): String = {
      t.showShapeTyping
    }
  }

  implicit def monoidShapeTyping = new Monoid[ShapeTyping] {
    override def empty: ShapeTyping = emptyShapeTyping

    override def combine(
      t1: ShapeTyping,
      t2: ShapeTyping): ShapeTyping =
      ShapeTyping(t1.t.combineTyping(t2.t))
  }

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping = {
    ShapeTyping(Typing.combineTypings(ts.map(_.t)))
  }

  implicit def showPair = new Show[(ShapeTyping, Evidences)] {
    def show(e: (ShapeTyping, Evidences)): String = {
      s"Typing: ${e._1.show}\n Evidences:\n${e._2.show}"
    }
  }

}
