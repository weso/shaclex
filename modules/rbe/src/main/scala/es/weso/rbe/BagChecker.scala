package es.weso.rbe
import es.weso.collection._
//import es.weso.validating._

trait BagChecker[A] {

  def rbe: Rbe[A]

  def check(bag: Bag[A], open: Boolean): Either[String, Bag[A]]

}