package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import es.weso.shex.extend.Extend
import es.weso.utils.EitherUtils._

abstract sealed trait ShapeExpr {
  def id: Option[ShapeLabel]
  def addId(lbl: ShapeLabel): ShapeExpr

  def showPrefixMap(pm: PrefixMap) = {
    import es.weso.shex.compact.CompactShow._
    showShapeExpr(this, pm)
  }

  def paths(schema: Schema): Either[String,List[Path]]
}

object ShapeExpr {
  def any: ShapeExpr = Shape.empty

  def fail: ShapeExpr = NodeConstraint.valueSet(List(), List())
}

case class ShapeOr(id: Option[ShapeLabel], shapeExprs: List[ShapeExpr]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: Schema): Either[String,List[Path]] = sequence(shapeExprs.map(_.paths(schema))).map(_.flatten)
}

case class ShapeAnd(id: Option[ShapeLabel], shapeExprs: List[ShapeExpr]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: Schema): Either[String,List[Path]] = sequence(shapeExprs.map(_.paths(schema))).map(_.flatten)

}

case class ShapeNot(id: Option[ShapeLabel], shapeExpr: ShapeExpr) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: Schema): Either[String,List[Path]] = shapeExpr.paths(schema)

}

case class NodeConstraint(
                           id: Option[ShapeLabel],
                           nodeKind: Option[NodeKind],
                           datatype: Option[IRI],
                           xsFacets: List[XsFacet],
                           values: Option[List[ValueSetValue]]
                         ) extends ShapeExpr {
  override def addId(lbl: ShapeLabel) = {
    this.copy(id = Some(lbl))
  }

  override def paths(schema: Schema): Either[String,List[Path]] = Right(List())

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
                  _extends: Option[List[ShapeLabel]],
                  semActs: Option[List[SemAct]],
                  annotations: Option[List[Annotation]]) extends ShapeExpr with Extend {
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
  private def extend(s: ShapeExpr): Option[List[ShapeLabel]] = s match {
    case s: Shape => s._extends
    case _ => None
  }

  private def expr(s: ShapeExpr): Option[TripleExpr] = s match {
    case s: Shape => s.expression
    case _ => None
  }

  def paths(schema: Schema): Either[String,List[Path]] = {
    def getPath(s: ShapeExpr): Option[List[Path]] = s match {
      case s: Shape => Some(s.expression.map(_.paths(schema)).getOrElse(List()))
      case _ => Some(List())
    }
    def combinePaths(p1: List[Path], p2: List[Path]): List[Path] = p1 ++ p2
    flattenShape(this, schema.getShape(_), extend, combinePaths, getPath).map(_.getOrElse(List()))
  }

  def extendExpression(schema: Schema): Either[String,Option[TripleExpr]] = {
    def combine(e1: TripleExpr, e2: TripleExpr): TripleExpr = {
      EachOf(None,List(e1,e2),None,None,None,None)
    }
    flattenShape(this, schema.getShape(_), extend, combine, expr)
  }

}

object Shape {
  def empty: Shape = Shape(
    id = None,
    virtual = None,
    closed = None,
    extra = None,
    expression = None,
    _extends = None,
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

  override def paths(schema: Schema): Either[String,List[Path]] =
    schema.getShape(reference) match {
      case None => Left(s"$reference not found in schema")
      case Some(se) => se.paths(schema)
    }

}

case class ShapeExternal(id: Option[ShapeLabel]) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
  override def paths(schema: Schema): Either[String,List[Path]] = Right(List())

}
