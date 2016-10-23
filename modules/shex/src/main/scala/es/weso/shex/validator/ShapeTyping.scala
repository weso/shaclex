package es.weso.shex.validator
import cats._, data._
import cats.implicits._
import es.weso.typing._
import es.weso.rdf.nodes._
import es.weso.shex._


case class ShapeTyping(
  t: Typing[RDFNode, ShapeType, ViolationError, String]
) {

  def getOkValues(node: RDFNode): Set[ShapeType] =
    t.getOkValues(node)

  def getFailedValues(node: RDFNode): Set[ShapeType] =
    t.getFailedValues(node)

  // TODO Review these definitions in case of anonymous shapes...
  def hasType(node: RDFNode, label: ShapeLabel): Boolean = {
    ! getOkValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def hasNoType(node: RDFNode, label: ShapeLabel): Boolean = {
    ! getFailedValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def addType(node: RDFNode, shapeType: ShapeType): ShapeTyping =
    ShapeTyping(t.addType(node,shapeType))

  def addEvidence(node: RDFNode, shapeType: ShapeType, evidence: String): ShapeTyping =
    ShapeTyping(t.addEvidence(node,shapeType,evidence))

  def addNotEvidence(node: RDFNode, shapeType: ShapeType, err: ViolationError): ShapeTyping =
    ShapeTyping(t.addNotEvidence(node,shapeType,err))

}

object ShapeTyping {
  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    def show(n: RDFNode) = s"$n"
  }

  implicit def showShapeTyping = new Show[ShapeTyping] {
    override def show(t: ShapeTyping): String = {
      s"ShapeTyping: $t"
    }
  }

  implicit def monoidShapeTyping = new Monoid[ShapeTyping] {

    override def empty: ShapeTyping =
      ShapeTyping(Typing.empty[RDFNode,ShapeType,ViolationError,String])
    override def combine(t1: ShapeTyping,
      t2: ShapeTyping): ShapeTyping =
      ShapeTyping(t1.t.combineTyping(t2.t))
  }

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping = {
    ShapeTyping(Typing.combineTypings(ts.map(_.t)))
  }
}
