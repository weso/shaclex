package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import cats._
import cats.data._
import cats.implicits._

import scala.util.{Either, Right}

// Calculates dependency graph
object Dependencies {

  def negCycles(schema: Schema): Either[String, Set[Set[ShapeExpr]]] = {
    depGraph(schema).map(_.negCycles)
  }

  def depGraph(schema: Schema): Either[String, DepGraph[ShapeExpr]] = {
    val emptyGraph: Either[String, DepGraph[ShapeExpr]] = Right(DepGraph.empty[ShapeExpr])
    schema.shapes match {
      case None => emptyGraph
      case Some(shapes) => shapes.foldRight(emptyGraph)(addDependency(schema))
    }
  }

  def addDependencies(graph: DepGraph[ShapeExpr], deps: List[(ShapeExpr,PosNeg,ShapeExpr)]): DepGraph[ShapeExpr] = {
    deps.foldRight(graph)(combine)
  }

  def combine(d: (ShapeExpr,PosNeg,ShapeExpr), g: DepGraph[ShapeExpr]): DepGraph[ShapeExpr] = {
    g.addEdge(d._1,d._2,d._3)
  }

  def addDependency(schema: Schema)(se: ShapeExpr, graph: Either[String, DepGraph[ShapeExpr]]): Either[String,DepGraph[ShapeExpr]] = {
    for {
      g <- graph
      deps <- dependencies(schema,se)
    } yield addDependencies(g, deps)
  }

  def dependencies(schema: Schema, shape: ShapeExpr): Either[String, List[(ShapeExpr,PosNeg,ShapeExpr)]] = {
    shape match {
      case s: ShapeAnd => Right(s.shapeExprs.map((shape, Pos,_)))
      case s: ShapeOr => Right(s.shapeExprs.map((shape, Pos,_)))
      case s: ShapeNot => Right(List((shape, Neg,s.shapeExpr)))
      case nc: NodeConstraint => Right(List())
      case s: Shape => {
        // TODO: Add negative dependencies to EXTRAs
        s.expression.map(dependencies(shape, schema,_)).getOrElse(Right(List()))
      }
      case s: ShapeRef => {
        val label = s.reference
        schema.getShape(label) match {
          case None => Left(s"Dependencies: Reference ${label} not found in schema")
          case Some(ref) => Right(List((shape, Pos,ref)))
        }
      }
      case s: ShapeExternal => Right(List())
    }
  }

  def dependencies(shape: ShapeExpr, schema: Schema, tripleExpr: TripleExpr): Either[String, List[(ShapeExpr, PosNeg,ShapeExpr)]] = {
    tripleExpr match {
      case t: EachOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions.map(dependencies(shape,schema,_)).sequenceU.map(_.flatten)
      }
      case t: OneOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions.map(dependencies(shape,schema,_)).sequenceU.map(_.flatten)
      }
      case i: Inclusion => {
        val label = i.include
        schema.getShape(label) match {
          case None => Left(s"Dependencies: Reference ${label} not found in schema")
          case Some(incl) => Right(List((shape,Pos,incl)))
        }
      }
      case tc: TripleConstraint => {
        tc.valueExpr match {
          case None => Right(List())
          case Some(s) => {
            if (tc.max == IntMax(0)) {
              // TODO: Should it be negative dependency?
              Right(List((shape,Pos,s)))
            } else {
              Right(List((shape,Pos,s)))
            }
          }
        }
      }
    }
  }
}