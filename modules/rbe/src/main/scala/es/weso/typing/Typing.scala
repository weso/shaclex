package es.weso.typing
import cats._, data._
import cats.implicits._

abstract class Typing[Key,Value,Error,Evidence] {

 type Evidences = List[Evidence]

 def hasType(key: Key, value: Value): Boolean = 
   getOkValues(key) contains value

 def getValues(key: Key): Map[Value,TypingResult[Error,Evidence]]

 def getOkValues(key: Key): Set[Value]

 def getEvidences(key: Key, value: Value): Option[List[Evidence]]

 def getFailedValues(key: Key): Set[Value]
  
  def addEvidences(
      key: Key, 
      value: Value, 
      es: Evidences): Typing[Key,Value,Error,Evidence]

  def addEvidence(key: Key, value: Value, 
      es: Evidence): Typing[Key,Value,Error,Evidence]
  
  def addNotEvidence(key: Key, value: Value, e: Error): Typing[Key,Value,Error,Evidence]

  
  def addType(key:Key, value:Value, 
      evidences: List[Evidence] = List()): Typing[Key,Value,Error,Evidence] =
        addEvidences(key,value,evidences)
        
  def combineTyping(t: Typing[Key,Value,Error,Evidence]): Typing[Key,Value,Error,Evidence] 
  
}

object Typing {
  
  def empty[Key,Value,Error,Evidence]: Typing[Key,Value,Error, Evidence] = {
//    val m: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = Map()
    TypingMap[Key,Value,Error,Evidence](Map())
  }
  
  def combineTypings[Key, Value, Error, Evidence](
        ts: Seq[Typing[Key,Value,Error,Evidence]]): Typing[Key,Value,Error,Evidence] = {
    val zero : Typing[Key,Value,Error,Evidence] = Typing.empty
    ts.foldLeft(zero)(_.combineTyping(_))
  }
}

