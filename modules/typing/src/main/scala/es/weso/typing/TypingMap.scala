package es.weso.typing
import cats._, data._
import cats.implicits._
import TypingResult._

case class TypingMap[Key, Value, Err, Evidence](
  m: Map[Key, Map[Value, TypingResult[Err, Evidence]]])
  extends Typing[Key, Value, Err, Evidence] {

  override def allOk: Boolean = {
    m.keys.map(key => getFailedValues(key).isEmpty).forall(identity)
  }

  override def getFailed: List[Key] = {
    m.keys.filter(key => !getFailedValues(key).isEmpty).toList
  }


  override def getKeys: Seq[Key] =
    m.keys.toSeq

  override def getValues(key: Key): Map[Value, TypingResult[Err, Evidence]] =
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

  def getResult(key: Key, value: Value): Option[TypingResult[Err, Evidence]] = {
    m.get(key) match {
      case None => None
      case Some(mm) => mm.get(value)
    }
  }

  def firstEvidences(es: List[Evidence]): TypingResult[Err, Evidence] = {
    TypingResult(Validated.valid(es))
  }

  def firstNotEvidence(e: Err): TypingResult[Err, Evidence] = {
    TypingResult(Validated.invalid(NonEmptyList.of(e)))
  }
  override def addEvidences(key: Key, value: Value,
    es: List[Evidence]): Typing[Key, Value, Err, Evidence] = {
    val newTyping: Map[Key, Map[Value, TypingResult[Err, Evidence]]] = m.updated(
      key,
      if (m.get(key).isDefined) {
        val valueMap = m(key)
        valueMap.updated(
          value,
          if (valueMap.get(value).isDefined) {
            valueMap(value).addEvidences(es)
          } else {
            TypingResult(Validated.valid(es))
          })
      } else
        (Map(value -> firstEvidences(es))))
    TypingMap(newTyping)
  }

  override def addEvidence(key: Key, value: Value, e: Evidence): Typing[Key, Value, Err, Evidence] =
    addEvidences(key, value, List(e))

  override def getMap = m

  override def addNotEvidence(key: Key, value: Value, e: Err): Typing[Key, Value, Err, Evidence] = {
    val newTyping: Map[Key, Map[Value, TypingResult[Err, Evidence]]] = m.updated(
      key,
      if (m.get(key).isDefined) {
        val valueMap = m(key)
        valueMap.updated(
          value,
          if (valueMap.get(value).isDefined) {
            valueMap(value).addNotEvidence(e)
          } else {
            TypingResult(Validated.invalid(NonEmptyList.of(e)))
          })
      } else
        (Map(value -> firstNotEvidence(e))))
    TypingMap(newTyping)
  }

  // TODO
  override def combineTyping(
    t: Typing[Key, Value, Err, Evidence]): Typing[Key, Value, Err, Evidence] = {
    t match {
      case tm: TypingMap[Key, Value, Err, Evidence] => {
        val r: Map[Key, Map[Value, TypingResult[Err, Evidence]]] =
          m combine tm.m
        TypingMap(r)
      }

      case _ => throw new Exception("Unsupported combination of different typing maps")
    }
  }

}
