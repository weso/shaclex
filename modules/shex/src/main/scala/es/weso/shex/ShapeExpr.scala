package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI

abstract sealed trait ShapeExpr {
  def id: Option[ShapeLabel]
  def addId(lbl: ShapeLabel): ShapeExpr

  def showPrefixMap(pm: PrefixMap) = {
    import es.weso.shex.compact.CompactShow._
    showShapeExpr(this, pm)
  }
}

object ShapeExpr {
  def any: ShapeExpr = Shape.empty

  def fail: ShapeExpr = NodeConstraint.valueSet(List(), List())
}

case class ShapeOr(id: Option[ShapeLabel], shapeExprs: List[ShapeExpr]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

}

case class ShapeAnd(id: Option[ShapeLabel], shapeExprs: List[ShapeExpr]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
}

case class ShapeNot(id: Option[ShapeLabel], shapeExpr: ShapeExpr) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
}

case class NodeConstraint(
                           id: Option[ShapeLabel],
                           nodeKind: Option[NodeKind],
                           datatype: Option[IRI],
                           xsFacets: List[XsFacet],
                           values: Option[List[ValueSetValue]]
                         ) extends ShapeExpr {
  override def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

}

object NodeConstraint {

  def empty = NodeConstraint(
    id = None,
    nodeKind = None,
    datatype = None,
    xsFacets = List(),
    values = None)

  def nodeKind(nk: NodeKind, facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(
      nodeKind = Some(nk),
      xsFacets = facets)

  def nodeKind(idLabel: Option[ShapeLabel], nk: NodeKind, facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(
      id = idLabel,
      nodeKind = Some(nk),
      xsFacets = facets)

  def datatype(
                dt: IRI,
                facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(
      datatype = Some(dt),
      xsFacets = facets)

  def valueSet(
                vs: List[ValueSetValue],
                facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(
      values = Some(vs),
      xsFacets = facets)
}

case class Shape(
                  id: Option[ShapeLabel],
                  virtual: Option[Boolean],
                  closed: Option[Boolean],
                  extra: Option[List[IRI]], // TODO: Extend extras to handle Paths?
                  expression: Option[TripleExpr],
                  inherit: Option[ShapeLabel],
                  semActs: Option[List[SemAct]],
                  annotations: Option[List[Annotation]]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  def isVirtual: Boolean =
    virtual.getOrElse(Shape.defaultVirtual)

  def isClosed: Boolean =
    closed.getOrElse(Shape.defaultClosed)

  // Converts IRIs to direct paths
  def extraPaths =
    extra.getOrElse(List()).map(Direct(_))

  def isEmpty: Boolean = this == Shape.empty

  // def tripleExpr = expression.getOrElse(TripleExpr.any)

}

object Shape {
  def empty: Shape = Shape(
    id = None,
    virtual = None,
    closed = None,
    extra = None,
    expression = None,
    inherit = None,
    semActs = None,
    annotations = None
  )

  def defaultVirtual = false
  def defaultClosed = false
  def defaultExtra = List[IRI]()
  def defaultInherit = List[ShapeLabel]()
  def defaultSemActs = List[SemAct]()

  def expr(te: TripleExpr): Shape = {
    Shape.empty.copy(expression = Some(te))
  }
}

case class ShapeRef(reference: ShapeLabel) extends ShapeExpr {
  def id = None
  def addId(lbl: ShapeLabel) = this

}

case class ShapeExternal(id: Option[ShapeLabel]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

}
