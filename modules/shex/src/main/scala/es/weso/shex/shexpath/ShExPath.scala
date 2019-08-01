package es.weso.shex.shexpath

import cats.data.NonEmptyList
import es.weso.shex.ShapeLabel

/**
* The following code is experimental.
 * It will provide an implementation of: https://shexspec.github.io/spec/ShExPath
 */

sealed trait ShExPath extends Product with Serializable

case class AbsolutePath(rpath: RelativePath) extends ShExPath
case class RelativePath(step: NonEmptyList[StepExpr]) extends ShExPath

sealed trait StepExpr extends Product with Serializable
case class ShapeAndPath(exprIndex: ExprIndex) extends StepExpr
case class ShapeOrPath(exprIndex: ExprIndex) extends StepExpr
case class ShapeNotPath(exprIndex: ExprIndex) extends StepExpr
case class ShapePath(exprIndex: ExprIndex) extends StepExpr
case class NodeConstraintPath(exprIndex: ExprIndex) extends StepExpr
case class EafOfPath(exprIndex: ExprIndex) extends StepExpr
case class OneOfPath(exprIndex: ExprIndex) extends StepExpr
case class TripleConstraintPath(exprIndex: ExprIndex) extends StepExpr

sealed trait ExprIndex extends Product with Serializable
case class ShapeExprIndex(value: Either[Int, ShapeLabel]) extends ExprIndex
case class TripleExprIndex(value: Either[Int,ShapeLabel]) extends ExprIndex

