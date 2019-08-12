package es.weso.rdf.sgraph

sealed trait DotShape {
  def name: String
}
case object Ellipse extends DotShape {
  override def name = "ellipse"
}
case object Circle extends DotShape {
  override def name = "circle"
}
case object Rectangle extends DotShape {
  override def name = "rectangle"
}

sealed trait DotColor {
  def name: String
}
case object Black extends DotColor {
  override def name = "black"
}
case object Yellow extends DotColor {
  override def name = "yellow"
}
case object Gray extends DotColor {
  override def name = "gray"
}

sealed trait DotStyle {
  def name: String
}
case object Filled extends DotStyle {
  override def name = "filled"
}
case object Solid extends DotStyle {
  override def name = "solid"
}

case class DotNodePreferences(shape: DotShape, style: DotStyle, color: DotColor)

case class RDFDotPreferences(irisPrefs: DotNodePreferences,
                             bnodesPrefs: DotNodePreferences,
                             literalPrefs: DotNodePreferences)

object RDFDotPreferences {
  lazy val defaultIRIsPrefs = DotNodePreferences(Ellipse,Solid,Black)
  lazy val defaultBNodePrefs = DotNodePreferences(Circle,Filled,Gray)
  lazy val defaultLiteralPrefs = DotNodePreferences(Rectangle, Filled, Yellow)
  lazy val defaultRDFPrefs =
    RDFDotPreferences(
      irisPrefs = defaultIRIsPrefs,
      bnodesPrefs = defaultBNodePrefs,
      literalPrefs = defaultLiteralPrefs
    )
}