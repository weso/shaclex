package es.weso.checking
import cats._, data._
import implicits._
import es.weso.utils.Read

case class CheckVal[A, Err, Evidence](t: ValidatedNel[Err,(A,Evidence)]) {
  def fold[B](ifOK: ((A,Evidence)) => B, ifErr: NonEmptyList[Err] => B): B = 
    t.fold(ifErr, ifOK)
}

object CheckVal {

  def ok[A,Err,Evidence](x: A, r: Evidence): CheckVal[A,Err,Evidence] =
    CheckVal(Validated.valid((x,r)))

  def ok[A,Err,Evidence: Read](x: A, msg: String): CheckVal[A,Err,Evidence] =
    CheckVal(Validated.valid((x,Read[Evidence].read(msg))))

  def errString[A,Err:Read,Evidence](msg: String): CheckVal[A,Err,Evidence] =
    CheckVal(Validated.invalidNel(Read[Err].read(msg)))
}
