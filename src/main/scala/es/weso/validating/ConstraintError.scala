package es.weso.validating

class ConstraintError(msg:String) extends Exception(msg)

case class All_SomeNotValid(msg:String) extends ConstraintError(msg)
case class Some_NoOneValid(msg:String) extends ConstraintError(msg)
case class OneOf_NoOneValid(msg:String) extends ConstraintError(msg)
case class OneOf_MoreThanOneValid(msg:String) extends ConstraintError(msg)

