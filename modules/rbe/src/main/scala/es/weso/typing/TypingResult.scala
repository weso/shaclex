package es.weso.typing
import cats._, data._
import implicits._

case class TypingResult[Error,Evidence](t: ValidatedNel[Error,List[Evidence]]) {
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
    
 def show(implicit Error: Show[Error], Evidence: Show[Evidence]): String = { 
    t.fold(errors => {
       s"Error: ${showErrors(errors)}" 
    },
    es => {
      s"Evidences: ${showEvidences(es)}" 
    })
  }
  
  def tab = " "
  
  def showErrors[Error: Show](es: NonEmptyList[Error]): String = { 
    es.toList.map(_.show).mkString("\n" + tab)
  }
  
  def showEvidences[Evidence: Show](es: List[Evidence]): String = {
    es.map(_.show).mkString("\n" + tab)
  }
}
