package es.weso.shex.validator
import cats._, data._
import cats.implicits._
import es.weso.mytyping._
import es.weso.rdf.nodes._
import es.weso.shex._


case class ShapeTyping(
  t: Typing[RDFNode, ShapeLabel, ViolationError, String]
) {

  def getOkValues(node: RDFNode): Set[ShapeLabel] =
    t.getOkValues(node)

  def getFailedValues(node: RDFNode): Set[ShapeLabel] =
    t.getFailedValues(node)

  def addType(node: RDFNode, label: ShapeLabel): ShapeTyping =
    ShapeTyping(t.addType(node,label))

  def addEvidence(node: RDFNode, label: ShapeLabel, evidence: String): ShapeTyping =
    ShapeTyping(t.addEvidence(node,label,evidence))

  def addNotEvidence(node: RDFNode, label: ShapeLabel, err: ViolationError): ShapeTyping =
    ShapeTyping(t.addNotEvidence(node,label,err))

}

object ShapeTyping {
  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    def show(n: RDFNode) = s"$n"
  }
  implicit lazy val showShapeLabel: Show[ShapeLabel] = new Show[ShapeLabel] {
    def show(n: ShapeLabel) = s"$n"
  }

  implicit def showShapeTyping = new Show[ShapeTyping] {
    override def show(t: ShapeTyping): String = {
      s"ShapeTyping: $t"
    }
  }

  implicit def monoidShapeTyping = new Monoid[ShapeTyping] {

    override def empty: ShapeTyping =
      ShapeTyping(Typing.empty[RDFNode,ShapeLabel,ViolationError,String])
    override def combine(t1: ShapeTyping,
      t2: ShapeTyping): ShapeTyping =
      ShapeTyping(t1.t.combineTyping(t2.t))
  }

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping = {
    ShapeTyping(Typing.combineTypings(ts.map(_.t)))
  }
}
