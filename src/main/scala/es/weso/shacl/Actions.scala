package es.weso.shacl

case class Actions(as: List[String]) {
  def strs: List[String] = as
  def addAction(a: String): Actions = Actions(a :: as)
}

object Actions {
  def empty = Actions(List())
  def initial(msg: String): Actions = Actions(List(msg))
}