package es.weso.shex.converter

import es.weso.shex._

trait ShExSimplifier {
  def inlineInclusions(schema: Schema): Either[String,Schema] = {
    for {
      usages <- CountUsages.countUsages(schema)
      inlined <- inlineInclusionLabels(usages.toList.collect { case (lbl,c) if c == 1 => lbl}, schema)
    } yield {
      inlined
    }
  }

  def inlineInclusionLabels(ls: List[ShapeLabel], schema: Schema): Either[String, Schema] = {
    val zero: Either[String, Schema] = Right(schema)
    ls.foldLeft(zero)(inlineInclusion)
  }

  def inlineInclusion(r: Either[String,Schema], lbl: ShapeLabel): Either[String, Schema] = for {
    currentSchema <- r
    next <- inline(currentSchema, lbl)
  } yield next

  def inline(schema: Schema, lbl: ShapeLabel): Either[String, Schema] = for {
    te <- schema.tripleExprMap.get(lbl).toRight(s"Error obtaining label $lbl in map: ${schema.tripleExprMap}")
  } yield ???

}