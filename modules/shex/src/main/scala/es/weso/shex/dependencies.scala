package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import cats.implicits._
import es.weso.rdf.nodes.IRI

import scala.util.{Either, Right}

// Calculates dependency graph
object Dependencies {

  type ES[A] = Either[String, A]
  type Dep   = (ShapeLabel, PosNeg, ShapeLabel)
  type Deps  = List[Dep]

  private val noDeps: ES[Deps] = Right(List())

  /**
   * Returns the set of cycles that have a negative dependency in a schema
 *
   * @param schema the ShEx schema
   * @return either a string signaling an error or the set of negated cycles.
   *  If the set is empy, there are no negated cycles.
   */
  def negCycles(schema: Schema): ES[Set[Set[(ShapeLabel,ShapeLabel)]]] = {
    depGraph(schema).map(_.negCycles)
  }

  def oddNegCycles(schema: Schema): ES[Set[Set[(ShapeLabel,ShapeLabel)]]] = {
    depGraph(schema).map(_.oddNegCycles)
/*    for {
      dg <- depGraph(schema)
      negCycles = dg.negCycles.filter { nc => dg.countNegLinks(nc) % 2 == 1 }
    } yield negCycles */
  }



  /**
   * Returns the dependency graph of a schema
 *
   * @param schema the ShEx schema
   * @return either a string signalling an error or the dependency graph
   */
  def depGraph(schema: Schema): ES[DepGraph[ShapeLabel]] = {
    val emptyGraph: ES[DepGraph[ShapeLabel]] = Right(DepGraph.empty[ShapeLabel])
    val r = schema.shapes match {
      case None         => emptyGraph
      case Some(shapes) => shapes.foldRight(emptyGraph)(addDependency(schema))
    }
//    println(s"Dependency graph: $r")
    r
  }

  def addDependencies(graph: DepGraph[ShapeLabel], deps: Deps): DepGraph[ShapeLabel] = {
    deps.foldRight(graph)(combine)
  }

  def combine(d: Dep, g: DepGraph[ShapeLabel]): DepGraph[ShapeLabel] = {
//    println(s"Adding edge $d to graph: $g")
    g.addEdge(d._1, d._2, d._3)
  }

  def addDependency(schema: Schema)(se: ShapeExpr,
                                    graph: ES[DepGraph[ShapeLabel]]): Either[String, DepGraph[ShapeLabel]] = {
    for {
      g     <- graph
      label <- getLabel(se)
      deps  <- dependencies(schema, se, label, Pos)
    } yield addDependencies(g, deps)
  }

  def getLabel(se: ShapeExpr): ES[ShapeLabel] = {
    Either.fromOption(se.id, s"Shape $se has no label")
  }

  def dependencies(schema: Schema, shape: ShapeExpr, source: ShapeLabel, posNeg: PosNeg): ES[Deps] = {
    // println(s"Calculating dependencies of shape $shape with source label $source and posNeg $posNeg")
    shape match {
      case s: ShapeAnd =>
        s.shapeExprs.map(dependencies(schema, _, source, posNeg)).sequence[ES, Deps].map(_.flatten)

      case s: ShapeOr =>
        s.shapeExprs.map(dependencies(schema, _, source, posNeg)).sequence[ES, Deps].map(_.flatten)

      case s: ShapeNot =>
        dependencies(schema, s.shapeExpr, source, Neg)

      case _: NodeConstraint => noDeps

      case s: Shape =>
        for {
          depsExtras <- s.extra match {
            case None     => noDeps
            case Some(es) => s.expression.map(te => dependenciesExtras(es, schema, source, te, posNeg)).getOrElse(noDeps)
          }
          depsTripleExpr <- s.expression
            .map((tripleExpr: TripleExpr) => dependenciesTripleExpr(schema, source, tripleExpr, posNeg))
            .getOrElse(noDeps)
        } yield depsExtras ++ depsTripleExpr

      case s: ShapeRef      => {
        // println(s"Dependency: $source -$posNeg-> ${s.reference}")
        Right(List((source, posNeg, s.reference)))
      }
      case _: ShapeExternal => noDeps
    }
  }


  def dependenciesTripleExpr(schema: Schema, source: ShapeLabel, tripleExpr: TripleExpr, posNeg: PosNeg): ES[Deps] = {
    // println(s"Calculating dependencies of tripleExpr $tripleExpr with source label $source and posNeg $posNeg")
    tripleExpr match {
      case t: EachOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions
          .map((tripleExpr: TripleExpr) => dependenciesTripleExpr(schema, source, tripleExpr, posNeg))
          .sequence[ES, Deps]
          .map(_.flatten)
      }
      case t: OneOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions
          .map((tripleExpr: TripleExpr) => dependenciesTripleExpr(schema, source, tripleExpr, posNeg))
          .sequence[ES, Deps]
          .map(_.flatten)
      }
      case i: Inclusion => {
        Right(List((source, posNeg, i.include)))
      }
      case tc: TripleConstraint =>
        tc.valueExpr match {
          case None => noDeps
          case Some(ve) =>
            if (tc.max == IntMax(0)) {
              // TODO: Should it be negative dependency?
              dependencies(schema, ve, source, posNeg.change)
            } else {
              dependencies(schema, ve, source, posNeg)
            }
        }
      case _@tripleExpr => sys.error(s"Don't know how to handle TripleExpr $tripleExpr")
    }
  }

  def dependenciesExtras(es: List[IRI],
                         schema: Schema,
                         source: ShapeLabel,
                         tripleExpr: TripleExpr,
                         posNeg: PosNeg): ES[Deps] = {
    es.map(dependenciesExtra(_, schema, source, tripleExpr, posNeg)).sequence[ES,Deps].map(_.flatten)
  }

  def dependenciesExtra(e: IRI,
                        schema: Schema,
                        source: ShapeLabel,
                        tripleExpr: TripleExpr,
                        posNeg: PosNeg): ES[Deps] = {
    // println(s"Dependencies extra: $e")
    tripleExpr match {
      case t: EachOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions
          .map((tripleExpr: TripleExpr) => dependenciesExtra(e, schema, source, tripleExpr, posNeg))
          .sequence[ES, Deps]
          .map(_.flatten)
      }
      case t: OneOf => {
        // TODO: Take into account max cardinality = 0 as a negative dependency?
        t.expressions
          .map((tripleExpr: TripleExpr) => dependenciesExtra(e, schema, source, tripleExpr, posNeg))
          .sequence[ES, Deps]
          .map(_.flatten)
      }
      case i: Inclusion => {
        Right(List((source, posNeg, i.include)))
      }
      case tc: TripleConstraint => if (tc.predicate == e) {
        // println(s"DependenciesExtra, tripleConstraint: $tc with predicate $e")
        tc.valueExpr match {
          case None => noDeps
          case Some(ve) =>
            if (tc.max == IntMax(0)) {
              // TODO: Should it be negative dependency?
              dependencies(schema, ve, source, posNeg)
            } else {
              val newPosNeg = posNeg.change
              // println(s"Calling dependencies with $ve, source; $source, newPosNeg: $newPosNeg")
              dependencies(schema, ve, source, newPosNeg)
            }
        }
      } else noDeps
      case _@tripleExpr => sys.error(s"Don't know how to handle TripleExpr $tripleExpr")
    }
  }

}