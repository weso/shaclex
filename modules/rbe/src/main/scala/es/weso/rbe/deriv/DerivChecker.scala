package es.weso.rbe.deriv
import es.weso.rbe._
import es.weso.collection._

case class DerivChecker[A](rbe: Rbe[A]) extends BagChecker[A] {

  def check(bag: Bag[A], open: Boolean): Either[String, Bag[A]] = {
    val d = rbe.derivBag(bag, open, rbe.symbols)
    //    println(s"Deriv of $rbe against $bag = $d")
    if (d.nullable) Right(bag)
    else {
      d match {
        case Fail(msg) => Left(msg)
        case _ => Left(s"Non nullable expression: $d, bag: $bag, rbe: $rbe, open: $open")
      }
    }
  }

}