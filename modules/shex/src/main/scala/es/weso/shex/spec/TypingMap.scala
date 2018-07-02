package es.weso.shex.spec

import cats.data.{NonEmptyList, Validated}

sealed trait Info[+A] {
  def isOk: Boolean = this match {
    case c: Conformant[_] => true
    case _ => false
  }
}
case object Unknown extends Info[Nothing]
case class Conformant[A](evidences:List[A]) extends Info[A]
case class NonConformant[A](evidences:List[A]) extends Info[A]


case class TypingMap[Key, Value, Evidence](m: Map[Key, Map[Value, Info[Evidence]]]) {

  def addConformant(key: Key, value: Value): Either[String, TypingMap[Key,Value,Evidence]] = {
    m.get(key) match {
      case None =>
        Right(TypingMap(m.updated(key,Map(value -> Conformant(List())))))
      case Some(valueMap) => valueMap.get(value) match {
        case None =>
          Right(TypingMap(m.updated(key, valueMap.updated(value,Conformant(List())))))
        case Some(info) => info match {
          case Unknown => Right(TypingMap(m.updated(key, valueMap.updated(value, Conformant(List())))))
          case nc: NonConformant[_] => Left(s"Error adding value $value to key $key which was non conformant")
          case c: Conformant[_] => Right(TypingMap(m))
        }
      }
    }
  }
}

object TypingMap {
  def empty[Key,Value,Evidence]: TypingMap[Key,Value,Evidence] = TypingMap(Map())
}
