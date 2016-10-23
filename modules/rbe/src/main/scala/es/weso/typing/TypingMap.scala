package es.weso.typing
import cats._, data._
import implicits._

// Implementation of Typing based on Map
case class TypingMap[Key, Value, Error, Evidence](
  m: Map[Key, Map[Value, TypingResult[Error, Evidence]]])
    extends Typing[Key, Value, Error, Evidence] {

  override def getValues(key: Key): Map[Value, TypingResult[Error, Evidence]] =
    m.get(key).getOrElse(Map())

  override def getOkValues(key: Key): Set[Value] = {
    getValues(key).filter(p => p._2.isOK).keySet
  }

  override def getEvidences(key: Key, value: Value): Option[List[Evidence]] = for {
    mvalue <- m.get(key)
    typingResult <- mvalue.get(value)
    evidences <- typingResult.getEvidences
  } yield evidences

  def getFailedValues(key: Key): Set[Value] = {
    getValues(key).filter(p => !p._2.isOK).keySet
  }

  def getResult(key: Key, value: Value): Option[TypingResult[Error, Evidence]] = {
    m.get(key) match {
      case None     => None
      case Some(mm) => mm.get(value)
    }
  }

  def firstEvidences(es: List[Evidence]): TypingResult[Error, Evidence] = {
    TypingResult(Validated.valid(es))
  }

  def firstNotEvidence(e: Error): TypingResult[Error, Evidence] = {
    TypingResult(Validated.invalid(NonEmptyList.of(e)))
  }
  override def addEvidences(key: Key, value: Value,
                            es: List[Evidence]): Typing[Key, Value, Error, Evidence] = {
    val newTyping: Map[Key, Map[Value, TypingResult[Error, Evidence]]] = m.updated(key,
      if (m.get(key).isDefined) {
        val valueMap = m(key)
        valueMap.updated(value,
          if (valueMap.get(value).isDefined) {
            valueMap(value).addEvidences(es)
          } else {
            TypingResult(Validated.valid(es))
          })
      } else
        (Map(value -> firstEvidences(es))))
    TypingMap(newTyping)
  }

  override def addEvidence(key: Key, value: Value, e: Evidence): Typing[Key, Value, Error, Evidence] =
    addEvidences(key, value, List(e))

  override def addNotEvidence(key: Key, value: Value, e: Error): Typing[Key, Value, Error, Evidence] = {
    val newTyping: Map[Key, Map[Value, TypingResult[Error, Evidence]]] = m.updated(key,
      if (m.get(key).isDefined) {
        val valueMap = m(key)
        valueMap.updated(value,
          if (valueMap.get(value).isDefined) {
            valueMap(value).addNotEvidence(e)
          } else {
            TypingResult(Validated.invalid(NonEmptyList.of(e)))
          })
      } else
        (Map(value -> firstNotEvidence(e))))
    TypingMap(newTyping)
  }

  /*  implicit def semigroupTypingResult = new Semigroup[TypingResult[Error,Evidence]] {
    override def combine(
        t1: TypingResult[Error,Evidence], 
        t2: TypingResult[Error,Evidence]): TypingResult[Error,Evidence] =
      TypingResult(t1.t |+| t2.t) 
  } */

  implicit def monoidTypingResult = new Monoid[TypingResult[Error, Evidence]] {
    override def empty = TypingResult(Validated.valid(List()))
    override def combine(
      t1: TypingResult[Error, Evidence],
      t2: TypingResult[Error, Evidence]): TypingResult[Error, Evidence] =
      TypingResult(t1.t |+| t2.t)
  }

  // TODO
  override def combineTyping(t: Typing[Key, Value, Error, Evidence]): Typing[Key, Value, Error, Evidence] = {
    t match {
      case tm: TypingMap[Key, Value, Error, Evidence] => TypingMap(m combine tm.m)
      case _ => throw new Exception("Unsupported combination of different typing maps")
    }
  }

  implicit def showTypingMap(implicit Value: Show[Value], Error: Show[Error], Evidence: Show[Evidence]) = new Show[TypingMap[Key,Value,Error,Evidence]] {
    def show(t: TypingMap[Key, Value, Error, Evidence]): String =
      m.map(t => {
        val key = t._1
        val valuesMap = t._2
        valuesMap.map(s => {
          val value = s._1
          val result = s._2
          val showV = (if (result.isOK) "+" else "-") + value.show
          s"($key: $showV) -> ${result.show}"
        }).mkString("\n")
      }).mkString("\n")
  }

  def toString(implicit Value: Show[Value], Error: Show[Error], Evidence: Show[Evidence]): String = 
    this.show

}
