package es.weso.utils

import org.scalatest._

class SetUtilsTest extends FunSpec with Matchers {
  def shouldCalculatePSet[A](s: Set[A], expected: Stream[(Set[A],Set[A])]): Unit = {
    it(s"Should calculate pSet($s) and return $expected") {
      SetUtils.pSet(s) should contain theSameElementsAs(expected)
    }
  }

 describe("pSet") {
   shouldCalculatePSet(Set(1,2),
     Stream(
       (Set(1,2),Set[Int]()),
       (Set(1),Set(2)),
       (Set(2),Set(1)),
       (Set[Int](),Set(1,2))
    )
  )

   shouldCalculatePSet(Set(1,2,3),
     Stream(
       (Set(1,2,3),Set[Int]()),
       (Set(1,2),Set(3)),
       (Set(1,3),Set(2)),
       (Set(2,3),Set(1)),
       (Set(1),Set(2,3)),
       (Set(2),Set(1,3)),
       (Set(3),Set(1,2)),
       (Set[Int](),Set(1,2,3))
     )
   )

   shouldCalculatePSet(Set[Int](),
     Stream((Set[Int](),Set[Int]()))
   )

   shouldCalculatePSet(Set(1),
     Stream(
       (Set(1),Set[Int]()),
       (Set[Int](),Set(1))
     )
   )

 }
}