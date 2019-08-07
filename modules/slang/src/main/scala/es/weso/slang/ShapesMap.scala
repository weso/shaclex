package es.weso.slang
import es.weso.rdf.nodes.RDFNode

case class ShapesMap(map: Map[RDFNode, Value[SLang]]) {

  def validated(node: RDFNode, shape: SLang): Boolean = {
    // println(s"Checking validated $node/$shape")
    isConforming(node,shape) match {
      case Conforms | NotConforms | Inconsistent => true
      case Unknown => false
    }
  }

  def isConforms(node: RDFNode, shape: SLang): Boolean = {
    isConforming(node,shape) == Conforms
  }

  def isOk(node: RDFNode, shape: SLang): Boolean =
    isConforming(node,shape) match {
      case Conforms | Unknown => true
      case NotConforms | Inconsistent => false
    }

  def isConforming(node: RDFNode, shape: SLang): Val = {
    map.get(node) match {
      case None => Unknown
      case Some(v) => v.isConforming(shape)
    }
  }

  def addVal(node: RDFNode, shape: SLang, v: Val): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value(shape,v)))
    case Some(value) => ShapesMap(map.updated(node, value.addValue(shape,v)))
  }

  def conform(node: RDFNode, shape: SLang): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value.conform(shape)))
    case Some(value) => ShapesMap(map.updated(node, value.conform(shape)))
  }

  def unknown(node: RDFNode, shape: SLang): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value.unknown(shape)))
    case Some(value) => ShapesMap(map.updated(node, value.unknown(shape)))
  }

  def notConform(node: RDFNode, shape: SLang): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value.notConform(shape)))
    case Some(value) => ShapesMap(map.updated(node, value.notConform(shape)))
  }

  def addValue(node: RDFNode, value: Value[SLang]): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, value))
    case Some(otherValue) => ShapesMap(map.updated(node, value.combine(otherValue)))
  }

  def combine(other: ShapesMap): ShapesMap = {
    val zero: ShapesMap = this
    def comb(rest: ShapesMap, pair: (RDFNode, Value[SLang])): ShapesMap = {
      val (node,value) = pair
      rest.addValue(node, value)
    }
    other.map.foldLeft(zero)(comb)
  }

  override def toString: String = {
    map.map { case (node,values) => values.m.map { case (s,v) => showLine(node, s, v) } }.flatten.mkString("\n")
  }

  private def showLine(node: RDFNode, s: SLang, value: Val): String =
    s"$node/$s: $value"
}

object ShapesMap {
  def empty: ShapesMap = ShapesMap(Map())
}
