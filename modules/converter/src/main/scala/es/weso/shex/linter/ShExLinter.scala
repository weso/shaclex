package es.weso.shex.linter

import es.weso.depgraphs.DepGraph
import es.weso.shex.{Dependencies, Inclusion, Schema, Shape, ShapeExpr, ShapeLabel}
import cats.syntax.all._

object ShExLinter {

  def inlineInclusions(schema: Schema): Either[String, Schema] = {
    for {
      // _ <- { pprint.log(schema, "Schema"); Right(()) }
      // usages  <- CountUsages.countUsages(schema)
      depGraph <- Dependencies.depGraph(schema)
      embedablePairs <- searchEmbeddable(depGraph)
      // _ <- { pprint.log(depGraph.showEdges(), "Dependencies"); Right(()) }
      // _ <- { pprint.log(embedablePairs, "Embeddable pairs"); Right(()) }
      newSchema <- {
        def cmb(schema: Schema, pair: (ShapeLabel,ShapeLabel)): Either[String, Schema] = {
          val (embedNode, node) = pair
          embed(schema,node,embedNode)
        }
        embedablePairs.foldM(schema)(cmb)
      }
      // inlined <- inlineInclusionLabels(usages.toList.collect { case (lbl, counter) if counter == 1 && lbl.toRDFNode.isBNode => lbl }, schema)
    } yield {
      newSchema
    }
  }

  /**
   * Search nodes that are embeddable, which have 2 conditions:
   *  - They must be blank nodes
   *  - Only one shape depends on them
   * @param depGraph
   * @return a list of pairs where the first element is the embeddable and the second is the dependant node
   */
  private def searchEmbeddable(depGraph: DepGraph[ShapeLabel]): Either[String, List[(ShapeLabel, ShapeLabel)]] = {

    def hasOneDependency(node: ShapeLabel): Either[String, Option[(ShapeLabel, ShapeLabel)]] =
      depGraph.inEdges(node).map(ds => if (ds.size == 1) Some((node,ds.head._1)) else None)

    val bNodes = depGraph.nodes.toList.filter(_.toRDFNode.isBNode)
    bNodes.map(hasOneDependency(_)).sequence.map(_.flatten)
  }

  private def embed(schema: Schema, node: ShapeLabel, embedLabel: ShapeLabel): Either[String,Schema] = for {
    expr <- schema.getShape(node)
    embedExpr <- schema.getShape(embedLabel)
    newExpr <- Right(replaceShapeExpr(expr, embedLabel, embedExpr))
    newSchema1 <- Right(removeShape(schema,embedExpr))
    newSchema <- Right(replaceSchema(newSchema1, expr, newExpr))
  } yield newSchema

  private def replaceSchema(schema: Schema, expr: ShapeExpr, newExpr: ShapeExpr): Schema = {
    schema.copy(shapes = schema.shapes.map(ls => {
      replaceList(ls, expr, newExpr)
    }))
  }

  private def removeShape(schema: Schema, expr: ShapeExpr): Schema = {
    schema.copy(shapes = schema.shapes.map(_.filterNot(_ == expr)))
  }

  private def replaceShapeExpr(expr: ShapeExpr, node: ShapeLabel, newExpr: ShapeExpr): ShapeExpr = expr match {
    case s: Shape if s.expression == Some(Inclusion(node)) => newExpr match {
      case ns: Shape => s.copy(expression = ns.expression)
      case _ => expr
    }
    case _ => expr
  }

  private def replaceList[A](xs: List[A], x: A, newX: A): List[A] = xs.map(v => if (v == x) newX else v)

  /*
  private def inlineInclusionLabels(ls: List[ShapeLabel], schema: Schema): Either[String, Schema] = {
    val zero: Either[String, Schema] = Right(schema)
    ls.foldLeft(zero)(inlineInclusion)
  }

  private def inlineInclusion(r: Either[String, Schema], lbl: ShapeLabel): Either[String, Schema] =
    for {
      currentSchema <- r
      next          <- inline(currentSchema, lbl)
    } yield next

  private def inline(schema: Schema, lbl: ShapeLabel): Either[String, Schema] =
    for {
      _ <- { pprint.log(lbl, "Label to embed"); Right(()) }
      te <- schema.shapesMap.get(lbl).toRight(s"Error obtaining label $lbl in map: ${schema.shapesMap}")
      _ <- { pprint.log(te, "Triple Expr to embed"); Right(()) }
    } yield schema */

}
