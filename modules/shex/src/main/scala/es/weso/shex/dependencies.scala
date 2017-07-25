package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import cats._
import cats.data._
import cats.implicits._

import scala.util.{Either, Right}

// Calculates dependency graph
object Dependencies {

  /**
  * Returns the set of cycles that have a negative dependency in a schema
  * @param schema the ShEx schema
  * @return either a string signaling an error or the set of negated cycles.
  *  If the set is empy, there are no negated cycles.
  */
  def negCycles(schema: Schema): Either[String, Set[Set[ShapeLabel]]] = {
    depGraph(schema).map(_.negCycles)
  }

  /**
    * Returns the dependency graph of a schema
    * @param schema the ShEx schema
    * @return either a string signalling an error or the dependency graph
    */
  def depGraph(schema: Schema): Either[String, DepGraph[ShapeLabel]] = {
    val emptyGraph: Either[String, DepGraph[ShapeLabel]] = Right(DepGraph.empty[ShapeLabel])
    schema.shapes match {
      case None => emptyGraph
      case Some(shapes) => shapes.foldRight(emptyGraph)(addDependency(schema))
    }
  }

  def addDependencies(graph: DepGraph[ShapeLabel], deps: List[(ShapeLabel, PosNeg, ShapeLabel)]): DepGraph[ShapeLabel] = {
    deps.foldRight(graph)(combine)
  }

  def combine(d: (ShapeLabel, PosNeg, ShapeLabel), g: DepGraph[ShapeLabel]): DepGraph[ShapeLabel] = {
    g.addEdge(d._1, d._2, d._3)
  }

  def addDependency(schema: Schema)(se: ShapeExpr, graph: Either[String, DepGraph[ShapeLabel]]): Either[String, DepGraph[ShapeLabel]] = {
    for {
      g <- graph
      label <- getLabel(se)
      deps <- dependencies(schema, se, label, Pos)
    } yield addDependencies(g, deps)
  }

  def getLabel(se: ShapeExpr): Either[String, ShapeLabel] = {
    Either.fromOption(se.id, s"Shape $se has no label")
  }

  def dependencies(schema: Schema,
                   shape: ShapeExpr,
                   source: ShapeLabel,
                   posNeg: PosNeg): Either[String, List[(ShapeLabel, PosNeg, ShapeLabel)]] = {
    shape match {
      case s: ShapeAnd =>
        s.shapeExprs.map(dependencies(schema,_,source, posNeg)).sequenceU.map(_.flatten)

      case s: ShapeOr =>
        s.shapeExprs.map(dependencies(schema,_,source, posNeg)).sequenceU.map(_.flatten)

      case s: ShapeNot =>
        dependencies(schema,s,source,Neg)

      case nc: NodeConstraint => Right(List())

      case s: Shape => {
        // TODO: Add negative dependencies to EXTRAs
        s.expression.map(dependenciesTripleExpr(source, schema, _, posNeg)).getOrElse(Right(List()))
      }

      case s: ShapeRef => Right(List((source, posNeg, s.reference)))
      case s: ShapeExternal => Right(List())
    }
  }

  def dependenciesTripleExpr(source: ShapeLabel, schema: Schema, tripleExpr: TripleExpr, posNeg: PosNeg): Either[String, List[(ShapeLabel, PosNeg, ShapeLabel)]] = {
    tripleExpr match {
      case t: EachOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions.map(dependenciesTripleExpr(source, schema, _, posNeg)).sequenceU.map(_.flatten)
      }
      case t: OneOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions.map(dependenciesTripleExpr(source, schema, _, posNeg)).sequenceU.map(_.flatten)
      }
      case i: Inclusion => {
        val label = i.include
        Right(List((source, posNeg, i.include)))
      }
      case tc: TripleConstraint =>
        tc.valueExpr match {
          case None => Right(List())
          case Some(ve) => if (tc.max == IntMax(0)) {
            // TODO: Should it be negative dependency?
            dependencies(schema,ve,source, posNeg.change)
          } else {
            dependencies(schema,ve,source, posNeg.change)
          }
        }
    }
  }
}
