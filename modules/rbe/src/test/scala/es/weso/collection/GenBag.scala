package es.weso.collection
import org.scalatest._
import org.scalacheck._

import org.scalacheck.Gen._

trait GenBag {

  val letter = Gen.oneOf('a', 'b', 'c', 'd', 'e')

  val bagOfCharFromContainer: Gen[Bag[Char]] =
    Gen.containerOf[List, Char](letter).map(x => Bag.toBag(x))

}