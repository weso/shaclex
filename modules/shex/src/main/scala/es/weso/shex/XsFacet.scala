package es.weso.shex

import es.weso.rdf.operations.Comparisons.NumericLiteral

sealed trait XsFacet {
  val fieldName: String
}

sealed trait StringFacet extends XsFacet

case class Length(v: Int) extends StringFacet {
  val fieldName = "length"
}

case class MinLength(v: Int) extends StringFacet {
  val fieldName = "minlength"
}

case class MaxLength(v: Int) extends StringFacet {
  val fieldName = "maxlength"
}

case class Pattern(p: String, flags: Option[String]) extends StringFacet {
  val fieldName = "pattern"
}

sealed trait NumericFacet extends XsFacet

case class MinInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "mininclusive"
}
case class MinExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "minexclusive"
}
case class MaxInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxinclusive"
}
case class MaxExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxexclusive"
}
case class TotalDigits(n: Int) extends NumericFacet {
  val fieldName = "totaldigits"
}
case class FractionDigits(n: Int) extends NumericFacet {
  val fieldName = "fractiondigits"
}

