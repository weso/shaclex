package es.weso.validating

class ValidatingError(msg:String) extends Exception(msg)

// case class MsgError(msg:String) extends ValidatingError(msg)
case class OneOfWithSeveralValid[A,B](valids: NDResponse[A,B]) extends ValidatingError("More than one alternative in oneOf is valid")
case object NoneValid extends ValidatingError("None Valid")
case class Unsupported(msg:String) extends ValidatingError(msg)
