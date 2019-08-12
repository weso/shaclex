package es.weso.shacl

object Shacl {
  case object Unbounded
  lazy val defaultMin = 0
  lazy val defaultMax = Unbounded
  lazy val defaultFormat = "TURTLE"
}
