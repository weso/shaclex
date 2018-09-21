package es.weso.slang

case class Value[A](m: Map[A,Val]) {

  def isConforming(x: A): Val = m.getOrElse(x,Unknown)

  def addValue(x: A, value: Val): Value[A] = m.get(x) match {
    case None => Value(m.updated(x,value))
    case Some(otherValue) => Value(m.updated(x, otherValue.combineVal(value)))
  }

  def combine(otherValue: Value[A]): Value[A] = {
    val zero = this
    def comb(rest: Value[A], pair:(A,Val)): Value[A] = {
      val (a,v) = pair
      rest.addValue(a,v)
    }
    otherValue.m.foldLeft(zero)(comb)
  }

  def conform(x: A): Value[A] = addValue(x,Conforms)
  def notConform(x:A): Value[A] = addValue(x,NotConforms)
  def unknown(x: A): Value[A] = addValue(x,Unknown)

  def getVal(x:A): Val = m.get(x).getOrElse(Unknown)
}

object Value {
  def apply[A](x: A, v: Val): Value[A] = Value(Map(x -> v))
  def conform[A](x: A): Value[A] = Value(Map(x -> Conforms))
  def notConform[A](x:A): Value[A] = Value(Map(x -> NotConforms))
  def unknown[A](x: A): Value[A] = Value(Map(x -> Unknown))
}