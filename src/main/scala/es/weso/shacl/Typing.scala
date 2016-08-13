package es.weso.shacl
import cats._, data._
import cats.implicits._
import cats.syntax.all._

/**
 * Trait that defines a generic typing
 */
abstract class Typing[
  Key: Show, 
  Value: Show, 
  Error: Show, 
  Evidence: Show] {

implicit val showValue = implicitly[Show[Value]]
implicit val showError = implicitly[Show[Error]]
implicit val showEvidence = implicitly[Show[Evidence]]

 type Evidences = List[Evidence]

 def getValues(key: Key): Map[Value,TypingResult[Error,Evidence]]

 def getOkValues(key: Key): Set[Value]

 def getFailedValues(key: Key): Set[Value]

 def showResult(r: TypingResult[Error,Evidence]): String = { 
    r.t.fold(errors => {
       s"Incorrect. Errors: \n ${showErrors(errors)}" 
    },
    es => {
      s"Correct. Reasons: \n ${showEvidences(es)}" 
    })
  }
  
  def tab = " "
  
  def showErrors(es: NonEmptyList[Error]): String = { 
    es.unwrap.map(e => showError.show(e)).mkString("\n" + tab)
  }
  
  def showEvidences(es: List[Evidence]): String = {
    es.map(e => showEvidence.show(e)).mkString("\n" + tab)
  }
  
  def addEvidence(key: Key, value: Value, e: Evidence): Typing[Key,Value,Error,Evidence]

}

object Typing {
  def empty[Key: Show,Value: Show,Error: Show,Evidence: Show]: Typing[Key,Value,Error, Evidence] = {
    val m: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = Map()
    TypingMap(m)
  }
}

case class TypingResult[Error,Evidence](t: ValidatedNel[Error,List[Evidence]])  {
  def isOK = t.isValid
  
  def addEvidence(e: Evidence): TypingResult[Error,Evidence] = {
    val r: ValidatedNel[Error,List[Evidence]] = 
      t.fold(errors => throw new Exception(s"Error adding evidence $e to an error value ${errors}"), 
          es => Validated.Valid(e :: es))
    TypingResult(t)
  }
}

case class TypingMap[
  Key: Show,
  Value: Show,
  Error: Show,
  Evidence: Show](
    m: Map[Key, Map[Value,TypingResult[Error,Evidence]]]) 
    extends Typing[Key,Value,Error,Evidence] {
  
  override def getValues(key: Key): Map[Value,TypingResult[Error,Evidence]] = 
    m.get(key).getOrElse(Map())
  
  def getOkValues(key: Key): Set[Value] = {
    getValues(key).filter(p => p._2.isOK).keySet
  }
  
  def getFailedValues(key: Key): Set[Value] = {
    getValues(key).filter(p => !p._2.isOK).keySet
  }
  
  def getResult(key: Key, value: Value): Option[TypingResult[Error,Evidence]] = {
    m.get(key) match {
      case None => None
      case Some(mm) => mm.get(value)
    }
  }
  
  def firstEvidence(e: Evidence): TypingResult[Error,Evidence] = {
    TypingResult(Validated.Valid(List(e)))
  }
  
    
  override def addEvidence(key: Key, value: Value, e: Evidence): Typing[Key,Value,Error,Evidence] = {
    val newTyping: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = m.updated(key, 
        if (m.get(key).isDefined) {
          val valueMap = m(key)
          valueMap.updated(value,
              if (valueMap.get(value).isDefined) {
                valueMap(value).addEvidence(e)
              } else {
                TypingResult(Validated.Valid(List(e)))
              })
        }
        else
          (Map(value -> firstEvidence(e))))
    TypingMap(newTyping) 
  } 
  
  override def toString: String = {
    m.map(t => {
      val key = t._1
      val valuesMap = t._2
      valuesMap.map(s => {
        val value = s._1
        val result = s._2
        val showV = (if (result.isOK) "+" else "-" ) + showValue.show(value) 
        s"($key - $showV) -> ${showResult(result)}"
      }).mkString("\n")
    }).mkString("\n") 
  }

}



