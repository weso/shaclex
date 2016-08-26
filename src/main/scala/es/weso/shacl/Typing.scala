package es.weso.shacl
import cats._, data._
import cats.implicits._

abstract class Typing[
  Key: Show, 
  Value: Show, 
  Error: Show, 
  Evidence: Show] {

implicit val showValue = implicitly[Show[Value]]
implicit val showError = implicitly[Show[Error]]
implicit val showEvidence = implicitly[Show[Evidence]]

 type Evidences = List[Evidence]

 def hasType(key: Key, value: Value): Boolean = 
   getOkValues(key) contains value

 def getValues(key: Key): Map[Value,TypingResult[Error,Evidence]]

 def getOkValues(key: Key): Set[Value]

 def getEvidences(key: Key, value: Value): Option[List[Evidence]]

 def getFailedValues(key: Key): Set[Value]

 def showResult(r: TypingResult[Error,Evidence]): String = { 
    r.t.fold(errors => {
       s"Error: ${showErrors(errors)}" 
    },
    es => {
      s"Evidences: ${showEvidences(es)}" 
    })
  }
  
  def tab = " "
  
  def showErrors(es: NonEmptyList[Error]): String = { 
    es.toList.map(e => showError.show(e)).mkString("\n" + tab)
  }
  
  def showEvidences(es: List[Evidence]): String = {
    es.map(e => showEvidence.show(e)).mkString("\n" + tab)
  }
  
  def addEvidences(key: Key, value: Value, 
      es: List[Evidence]): Typing[Key,Value,Error,Evidence]

  def addEvidence(key: Key, value: Value, 
      es: Evidence): Typing[Key,Value,Error,Evidence]
  
  def addNotEvidence(key: Key, value: Value, e: Error): Typing[Key,Value,Error,Evidence]

  
  def addType(key:Key, value:Value, 
      evidences: List[Evidence] = List()): Typing[Key,Value,Error,Evidence] =
        addEvidences(key,value,evidences)
        
  def combineTyping(t: Typing[Key,Value,Error,Evidence]): Typing[Key,Value,Error,Evidence] 
  
}

object Typing {
  def empty[Key: Show,Value: Show,Error: Show,Evidence: Show]: Typing[Key,Value,Error, Evidence] = {
    val m: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = Map()
    TypingMap(m)
  }
  
  def combineTypings[
    Key:Show, Value: Show, Error: Show, Evidence: Show](
        ts: Seq[Typing[Key,Value,Error,Evidence]]): Typing[Key,Value,Error,Evidence] = {
    val zero : Typing[Key,Value,Error,Evidence] = Typing.empty
    ts.foldLeft(zero)(_.combineTyping(_))
  }
}

case class TypingResult[
  Error: Show,
  Evidence: Show](t: ValidatedNel[Error,List[Evidence]]) {
  def isOK = t.isValid
  
  def addEvidence(e: Evidence): TypingResult[Error,Evidence] = {
    addEvidences(List(e))
  }
  
  def addNotEvidence(e: Error): TypingResult[Error,Evidence] = {
    val r = t.fold(
        errors => Validated.invalid(e :: errors),
        ls => Validated.invalid(NonEmptyList.of(e))
    )
    TypingResult(t)
  }
  
  def addEvidences(es: List[Evidence]): TypingResult[Error,Evidence] = {
    val r = t.fold(errors => 
        throw new Exception(s"Error adding evidences $es to an error value ${errors}"), 
        ls => Validated.valid(ls ++ es))
    TypingResult(r)
  }
  
  def getEvidences: Option[List[Evidence]] =
    t.toOption
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
  
  def getResult(key: Key, value: Value): Option[TypingResult[Error,Evidence]] = {
    m.get(key) match {
      case None => None
      case Some(mm) => mm.get(value)
    }
  }
  
  def firstEvidences(es: List[Evidence]): TypingResult[Error,Evidence] = {
    TypingResult(Validated.valid(es))
  }
  
  def firstNotEvidence(e: Error): TypingResult[Error,Evidence] = {
    TypingResult(Validated.invalid(NonEmptyList.of(e)))
  }
  override def addEvidences(key: Key, value: Value, 
      es: List[Evidence]): Typing[Key,Value,Error,Evidence] = {
    val newTyping: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = m.updated(key, 
        if (m.get(key).isDefined) {
          val valueMap = m(key)
          valueMap.updated(value,
              if (valueMap.get(value).isDefined) {
                valueMap(value).addEvidences(es)
              } else {
                TypingResult(Validated.valid(es))
              })
        }
        else
          (Map(value -> firstEvidences(es))))
    TypingMap(newTyping) 
  } 
    
  override def addEvidence(key: Key, value: Value, e: Evidence): Typing[Key,Value,Error,Evidence] = 
    addEvidences(key,value,List(e)) 

  override def addNotEvidence(key: Key, value: Value, e: Error): Typing[Key,Value,Error,Evidence] = {
    val newTyping: Map[Key, Map[Value,TypingResult[Error,Evidence]]] = m.updated(key, 
        if (m.get(key).isDefined) {
          val valueMap = m(key)
          valueMap.updated(value,
              if (valueMap.get(value).isDefined) {
                valueMap(value).addNotEvidence(e)
              } else {
                TypingResult(Validated.invalid(NonEmptyList.of(e)))
              })
        }
        else
          (Map(value -> firstNotEvidence(e))))
    TypingMap(newTyping)
  }
    
    
/*  implicit def semigroupTypingResult = new Semigroup[TypingResult[Error,Evidence]] {
    override def combine(
        t1: TypingResult[Error,Evidence], 
        t2: TypingResult[Error,Evidence]): TypingResult[Error,Evidence] =
      TypingResult(t1.t |+| t2.t) 
  } */
  
  implicit def monoidTypingResult = new Monoid[TypingResult[Error,Evidence]] {
    override def empty = TypingResult(Validated.valid(List())) 
    override def combine(
        t1: TypingResult[Error,Evidence], 
        t2: TypingResult[Error,Evidence]): TypingResult[Error,Evidence] =
      TypingResult(t1.t |+| t2.t) 
  }
  
  // TODO
  override def combineTyping(t: Typing[Key,Value,Error,Evidence]): Typing[Key,Value,Error,Evidence] = {
    t match {
      case tm: TypingMap[Key,Value,Error,Evidence] => TypingMap(m combine tm.m) 
      case _ => throw new Exception("Unsupported combination of different typing maps")
    }
  }
  
  override def toString: String = {
    m.map(t => {
      val key = t._1
      val valuesMap = t._2
      valuesMap.map(s => {
        val value = s._1
        val result = s._2
        val showV = (if (result.isOK) "+" else "-" ) + showValue.show(value) 
        s"($key: $showV) -> ${showResult(result)}"
      }).mkString("\n")
    }).mkString("\n") 
  }

}



