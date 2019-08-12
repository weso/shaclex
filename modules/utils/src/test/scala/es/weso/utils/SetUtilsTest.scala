package es.weso.utils

import org.scalatest._

class SetUtilsTest extends FunSpec with Matchers {

 describe(s"pSet") {
  def shouldCalculatePSet[A](s: Set[A], expected: LazyList[(Set[A],Set[A])]): Unit = {
    it(s"Should calculate pSet($s) and return $expected") {
      SetUtils.pSet(s) should contain theSameElementsAs(expected)
    }
  }

 describe("pSet") {
   shouldCalculatePSet(Set(1,2),
     LazyList(
       (Set(1,2),Set[Int]()),
       (Set(1),Set(2)),
       (Set(2),Set(1)),
       (Set[Int](),Set(1,2))
    )
  )

   shouldCalculatePSet(Set(1,2,3),
     LazyList(
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
     LazyList((Set[Int](),Set[Int]()))
   )

   shouldCalculatePSet(Set(1),
     LazyList(
       (Set(1),Set[Int]()),
       (Set[Int](),Set(1))
     )
   )
 }
}

  describe(s"partition") {
    def shouldCalculatePartition[A](s: Set[A], n: Int, expected: LazyList[List[Set[A]]]): Unit = {
      it(s"Should calculate partition($s,$n) and return $expected") {
        SetUtils.partition(s,n) should contain theSameElementsAs (expected)
      }
    }

    shouldCalculatePartition(Set(1,2),1,
      LazyList(
        List(Set(1,2))
      )
    )

    shouldCalculatePartition(Set(1,2),2,
        LazyList(
          List(Set(1, 2), Set[Int]()),
          List(Set(2), Set(1)),
          List(Set(1), Set(2)),
          List(Set[Int](), Set(1, 2)))
    )

    shouldCalculatePartition(Set(1,2),3,
      LazyList(
        List(Set(1, 2), Set[Int](), Set[Int]()),
        List(Set(2), Set(1), Set[Int]()),
        List(Set(2), Set[Int](), Set(1)),
        List(Set(1), Set(2), Set[Int]()),
        List(Set(1), Set[Int](), Set(2)),
        List(Set[Int](), Set(1, 2), Set[Int]()),
        List(Set[Int](), Set(2), Set(1)),
        List(Set[Int](), Set(1), Set(2)),
        List(Set[Int](), Set[Int](), Set(1, 2))
      )
    )

    it(s"Should raise error with wrong argument") {
      an[Exception] should be thrownBy(SetUtils.partition(Set(1,2),0))
      an[Exception] should be thrownBy(SetUtils.partition(Set(1,2),-1))
    }

  }

}