package es.weso.shex.btValidator

sealed trait ShExErr

case class StringErr(msg: String) extends ShExErr