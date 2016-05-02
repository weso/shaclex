package es.weso.validating
import cats.Functor

class ValidatingError(msg:String) extends Exception(msg)

case class MsgError(msg:String) extends ValidatingError(msg)
case class OneOfWithSeveralValid[A,R[_]:Functor](valids: Responses[A,R]) extends ValidatingError("More than one alternative in oneOf is valid")
case object NoneValid extends ValidatingError("None Valid")
case class Unsupported(msg:String) extends ValidatingError(msg)
