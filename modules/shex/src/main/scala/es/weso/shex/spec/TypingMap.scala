package es.weso.shex.spec

sealed trait Info[+A] {
  def isOk: Boolean = this match {
    case c: Conformant[_] => true
    case _ => false
  }
}
case object Unknown extends Info[Nothing]
case object Testing extends Info[Nothing]
case class Conformant[A](evidences:List[A]) extends Info[A]
case class NonConformant[A](evidences:List[A]) extends Info[A]


case class TypingMap[Key, Value, Evidence](m: Map[Key, Map[Value, Info[Evidence]]]) {

  def conformingValues(key: Key): List[Value] = {
    m.get(key).getOrElse(Map()).collect { case (v, Conformant(_)) => v }.toList
  }

  def addConformant(key: Key,
                    value: Value,
                    es: List[Evidence]
                   ): Either[String, TypingMap[Key,Value,Evidence]] = {
    m.get(key) match {
      case None =>
        Right(TypingMap(m.updated(key,Map(value -> Conformant(es)))))
      case Some(valueMap) => valueMap.get(value) match {
        case None =>
          Right(TypingMap(m.updated(key, valueMap.updated(value,Conformant(es)))))
        case Some(info) => info match {
          case Unknown | Testing =>
            Right(TypingMap(m.updated(key, valueMap.updated(value, Conformant(es)))))
          case NonConformant(_) => Left(s"Error adding value $value to key $key which was non conformant")
          case Conformant(es1) =>
            Right(TypingMap(m.updated(key, valueMap.updated(value, Conformant(es1 ++ es)))))
        }
      }
    }
  }

  def addTesting(key: Key,
                 value: Value,
                 ): Either[String, TypingMap[Key,Value,Evidence]] = {
    m.get(key) match {
      case None =>
        Right(TypingMap(m.updated(key,Map(value -> Testing))))
      case Some(valueMap) => valueMap.get(value) match {
        case None =>
          Right(TypingMap(m.updated(key, valueMap.updated(value,Testing))))
        case Some(info) => info match {
          case Unknown | Testing =>
            Right(TypingMap(m.updated(key, valueMap.updated(value, Testing))))
          case NonConformant(_) => Right(this)
          case Conformant(es1) => Right(this)
        }
      }
    }
  }

  def addNonConformant(key: Key,
                       value: Value,
                       es: List[Evidence]
                      ): Either[String, TypingMap[Key,Value,Evidence]] = {
    m.get(key) match {
      case None =>
        Right(TypingMap(m.updated(key,Map(value -> NonConformant(es)))))
      case Some(valueMap) => valueMap.get(value) match {
        case None =>
          Right(TypingMap(m.updated(key, valueMap.updated(value,NonConformant(es)))))
        case Some(info) => info match {
          case Unknown | Testing =>
            Right(TypingMap(m.updated(key, valueMap.updated(value, NonConformant(es)))))
          case Conformant(_) =>
            Left(s"Error adding non-conformant value $value to key $key which was conformant")
          case NonConformant(es1) =>
            Right(TypingMap(m.updated(key,valueMap.updated(value, NonConformant(es1 ++ es)))))
        }
      }
    }
  }


}

object TypingMap {
  def empty[Key,Value,Evidence]: TypingMap[Key,Value,Evidence] = TypingMap(Map())
}
