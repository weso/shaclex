package es.weso.rdf.sgraph

/**
* More info: https://graphviz.gitlab.io/_pages/doc/info/shapes.html
 */
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
case object Square extends DotShape {
  override def name = "square"
}
case object Star extends DotShape {
  override def name = "star"
}


sealed trait DotColor { def name: String }
case object Red extends DotColor { override def name = "red" }
case object Green extends DotColor { override def name = "green" }
case object Blue extends DotColor { override def name = "blue" }
case object Black extends DotColor { override def name = "black" }
case object White extends DotColor { override def name = "white" }
case object Yellow extends DotColor { override def name = "yellow" }
case object Gray extends DotColor { override def name = "gray" }


/**
 * https://graphviz.gitlab.io/_pages/doc/info/attrs.html#d:style
 */
sealed trait DotStyle {
  def name: String
}
case object Filled extends DotStyle {
  override def name = "filled"
}
case object Solid extends DotStyle {
  override def name = "solid"
}

case class DotNodePreferences(shape: DotShape,
                              style: DotStyle,
                              color: DotColor
                             )

case class RDFDotPreferences(irisPrefs: DotNodePreferences,
                             bnodesPrefs: DotNodePreferences,
                             literalPrefs: DotNodePreferences)

object RDFDotPreferences {

  lazy val defaultIRIsPrefs = DotNodePreferences(Ellipse, Solid, Black)
  lazy val defaultBNodePrefs = DotNodePreferences(Circle, Filled, Gray)
  lazy val defaultLiteralPrefs = DotNodePreferences(Rectangle, Filled, Yellow)

  lazy val availableDotShapes: Set[DotShape] = Set(Rectangle,Circle,Ellipse,Square,Star)
  lazy val availableDotStyles: Set[DotStyle] = Set(Filled, Solid)
  lazy val availableDotColor: Set[DotColor] = Set(Red,Green,Blue,Black,White,Yellow,Gray)

  lazy val defaultRDFPrefs =
    RDFDotPreferences(
      irisPrefs = defaultIRIsPrefs,
      bnodesPrefs = defaultBNodePrefs,
      literalPrefs = defaultLiteralPrefs
    )
}