package es.weso.shex.linter

import es.weso.shex.{Schema, ShapeLabel}

trait ShExLinter {

  def inlineInclusions(schema: Schema): Either[String, Schema] = {
    for {
      _ <- { pprint.log(schema, "Schema"); Right(()) }
      usages  <- CountUsages.countUsages(schema)
      depGraph <- Dependencies.depGraph(schema)
      _ <- { pprint.log(depGraph.showEdges(), "Dependencies"); Right(()) }
      inlined <- inlineInclusionLabels(usages.toList.collect { case (lbl, counter) if counter == 1 && lbl.toRDFNode.isBNode => lbl }, schema)
    } yield {
      inlined
    }
  }

  def inlineInclusionLabels(ls: List[ShapeLabel], schema: Schema): Either[String, Schema] = {
    val zero: Either[String, Schema] = Right(schema)
    ls.foldLeft(zero)(inlineInclusion)
  }

  def inlineInclusion(r: Either[String, Schema], lbl: ShapeLabel): Either[String, Schema] =
    for {
      currentSchema <- r
      next          <- inline(currentSchema, lbl)
    } yield next

  def inline(schema: Schema, lbl: ShapeLabel): Either[String, Schema] =
    for {
      _ <- { pprint.log(lbl, "Label to embed"); Right(()) }
      te <- schema.shapesMap.get(lbl).toRight(s"Error obtaining label $lbl in map: ${schema.shapesMap}")
      _ <- { pprint.log(te, "Triple Expr to embed"); Right(()) }
    } yield schema

}
