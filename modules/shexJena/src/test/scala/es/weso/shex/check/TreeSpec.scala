package es.weso.shex.check

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

object TreeSpec extends Properties("Tree") {

  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree

  val genLeaf = Gen.const(Leaf)
  val genNode = for {
    v <- arbitrary[Int]
    left <- Gen.sized(h => Gen.resize(h/2, genTree))
    right <- Gen.sized(h => Gen.resize(h/2, genTree))
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = Gen.sized { height =>
    if (height <= 0) {
      genLeaf
    } else {
      Gen.oneOf(genLeaf, genNode)
    }
  }

  def elems(t:Tree):Int = t match {
    case Leaf => 0
    case Node(t1,t2,v) => elems(t1) + elems(t2)
  }

  property("elems(t) > 0") = forAll(genTree) { (t: Tree)  =>
    elems(t) >= 0
  }

}