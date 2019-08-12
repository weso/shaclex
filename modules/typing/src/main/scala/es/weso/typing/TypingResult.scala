package es.weso.typing
import cats._, data._
import cats.implicits._

case class TypingResult[Err, Evidence](
  t: ValidatedNel[Err, List[Evidence]]) {
  def isOK = t.isValid

  def addEvidence(e: Evidence): TypingResult[Err, Evidence] = {
    addEvidences(List(e))
  }

  def addNotEvidence(e: Err): TypingResult[Err, Evidence] = {
    val r = t.fold(
      errors => Validated.invalid(e :: errors),
      ls => Validated.invalid(NonEmptyList.of(e)))
    TypingResult(r)
  }

  def addEvidences(es: List[Evidence]): TypingResult[Err, Evidence] = {
    val r = t.fold(
      errors =>
        throw new Exception(s"Err adding evidences $es to an error value ${errors}"),
      ls => Validated.valid(ls ++ es))
    TypingResult(r)
  }

  def getEvidences: Option[List[Evidence]] =
    t.toOption

  def getErrors: Option[List[Err]] = {
    t.fold(es => Some(es.toList), _ => None)
  }

}

object TypingResult {

  implicit def monoidTypingResult[Err, Evidence] = new Monoid[TypingResult[Err, Evidence]] {
    override def empty: TypingResult[Err, Evidence] = {
      val e: List[Evidence] = List()
      TypingResult(Validated.valid(e))
    }

    override def combine(
      t1: TypingResult[Err, Evidence],
      t2: TypingResult[Err, Evidence]): TypingResult[Err, Evidence] = {
      TypingResult(t1.t |+| t2.t)
    }
  }

  implicit def showTypingResult[Err: Show, Evidence: Show] =
    new Show[TypingResult[Err, Evidence]] {
      override def show(r: TypingResult[Err, Evidence]): String =
        r.t.fold(
          errors => {
            s"Error: ${showErrors(errors)}"
          },
          es => {
            s"Evidences: ${showEvidences(es)}"
          })
    }

  def tab = " "

  def showErrors[Err: Show](es: NonEmptyList[Err]): String = {
    es.toList.map(e => Show[Err].show(e)).mkString("\n" + tab)
  }

  def showEvidences[Evidence: Show](es: List[Evidence]): String = {
    es.map(e => Show[Evidence].show(e)).mkString("\n" + tab)
  }

}
