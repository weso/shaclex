package es.weso.validating
import cats._
import cats.syntax._
import cats.implicits._
import Constraint._
import CheckedSub._
import ConstraintReason._
import org.scalatest._


class TypeCheckerTest extends FunSpec with Matchers with OptionValues {

  describe("Simple Type Checker using the validating library") {
    
/*  type Node = String
  type Label = String
  type Typing = Map[String, String]
  type CheckResult = CheckedVal[(Node,Label), Typing]
  type Context = Map[String,Seq[Action]]
  type Action = Either[Label,Pending[Node,Label]]

  def addType(node: Node, label:Label, t: Typing): Typing = {
    t + (node -> label)
  }
  
  def checkAndCombine[A,R](
      x: A, 
      check: A => CheckedVal[A,R], 
      comb: (A,CheckedInfo[A,R]) => CheckedInfo[A,R],
      msg: String): CheckedVal[A,R] = {
    val result = check(x)
    result.fold(
      (v,rs) => {
        val infos: Seq[CheckedInfo[A,R]] = rs.map(r => comb(x,r))
        val vs : Seq[R] = Seq()
        okSingle(x, vs, msg)
      }
    , (_,_) => result
    )  
  }
  
  
  case class Pending[Node,Label](
      label: Label, 
      pendingNode: Node, 
      pendingLabel: Label)
      
  def checkAction
           (node: Node, label: Label, ctx: Context, action: Action): CheckResult = {
   action match {
    case Left(lbl) => 
      if (label == lbl) {
       okSingle((node,label), Map(node -> label),s"$node has type $label")
      } else {
       errString((node,label),s"$node has type $lbl instead of $label")
      }
    case Right(Pending(lbl, pendingNode,pendingLabel)) => 
       checkAndCombine((node,label), 
         _ => checker(pendingNode,pendingLabel, ctx), 
         (p,t) => addType(p._1,p._2,t), 
        "Assigned type")
    }
 }
      
  def checker(node: Node, label: Label, ctx: Context): CheckResult = {
        println(s"Checking $node with $label")
        val value = ctx.get(node)
        value match {
          case None => errString((node,label), s"node doesn't appear in context")
          case Some(as) => {
            some(as.map(a => {
              val c = checkAction(node,label,ctx,a)
              println(s"Inside some...Check action($node,$label,ctx, $a)= $c")
              c
            }
            ))
          }
        }
      }

    implicit val typingMonoid = new Monoid[Typing] {
      override val empty: Typing = Map()
      override def combine(t1: Typing, t2: Typing): Typing = 
        t1 ++ t2
    }
    
    def checkSeq(vs: Seq[(Node,Label)], ctx: Context, t: Typing): CheckResult = {
      val zero: CheckResult = okEmpty() 
      val comb: ((Node,Label),CheckResult) => CheckResult = (p,current) => {
        val (node,label) = p
        all2(current,checker(node,label,ctx))
      }
      vs.foldRight(zero)(comb)
    }
      
    it("Should be able to type check single values") {
      val ctx : Context = Map("x1" -> Seq(Left("X")))
      val result = checker("x1","X",ctx)
      result.isOK should be(true)
    }
    
    it("Should fail to type check single values when value is not in context") {
      val ctx : Context = Map("x1" -> Seq(Left("X")))
      val result = checker("x2","X",ctx)
      result.isOK should be(false)
    }
    
    it("Should fail to type check single values when value has different label") {
      val ctx : Context = Map("x1" -> Seq(Left("X")))
      val result = checker("x1","Y",ctx)
      result.isOK should be(false)
    }
    
    it("Should type check pending values") {
      val ctx : Context = 
        Map("x1" -> Seq(Left("X")), 
            "x2" -> Seq(Right(Pending("Y","x1","X"))))
            
      val result = checker("x2","Y",ctx)
      result.isOK should be(true)
    }

    it("Should type check a value that depends on another value") {
      val ctx : Context = 
        Map("x1" -> Seq(Left("X")), 
            "x2" -> Seq(Right(Pending("Y","x1","X")))
           )
            
      val result = checker("x2","Y",ctx)
      result.isOK should be(true)
    }
    
    it("Should type check sequence of values with dependencies") {
      val ctx : Context = 
        Map("x1" -> Seq(Left("X")), 
            "x2" -> Seq(Right(Pending("Y","x1","X"))))
            
      val result = checkSeq(Seq(("x2","Y"),("x1","X")),ctx, Monoid[Typing].empty)
      result.isOK should be(true)
      result.responses.length should be(1)
      val typing = result.responses.head
      typing._2 should contain ("x2" -> "Y")
      typing._2 should contain ("x1" -> "X")
    }

    it("Should type check sequence of values with duplicates") {
      val ctx : Context = 
        Map("x1" -> Seq(Left("X")), 
            "x2" -> Seq(Left("X"),Left("Y")))
            
      val result = checkSeq(Seq(("x2","Y"),("x1","X")),ctx, Monoid[Typing].empty)
      println(s"Result: $result") 
      result.isOK should be(true)
      result.responses.length should be(1)
      val r = result.responses.head
      r._2 should contain ("x2" -> "Y")
      r._2 should contain ("x1" -> "X")
    }
    
    it("Should fail to type check sequence of values with duplicates when no one is satisfied") {
      val ctx : Context = 
        Map("x1" -> Seq(Left("X")), 
            "x2" -> Seq(Left("X"),Left("Y")))
            
      val result = checkSeq(Seq(("x2","Z"),("x1","X")),ctx, Monoid[Typing].empty)
      println(s"Result: $result") 
      result.isOK should be(false)
    }

    it("Should type check sequence of values with chained dependencies") {
      val ctx : Context = 
        Map("x0" -> Seq(Right(Pending("X","x1","X"))),
            "x1" -> Seq(Left("X")),
            "x2" -> Seq(Right(Pending("Y1","x0","Z")),
                        Right(Pending("Y2","x3","Z"))),
            "x3" -> Seq(Left("Z"))
           )
            
      val result = checkSeq(Seq(("x2","Y2"),("x1","X")),ctx, Monoid[Typing].empty)
      result.isOK should be(true)
      result.responses.length should be(1)
      val r = result.responses.head
      r._2 should contain ("x2" -> "Y2")
      r._2 should contain ("x3" -> "Z")
    }
    
    it("Should type check sequence of values with several paths") {
      val ctx : Context = 
        Map("x0" -> Seq(Right(Pending("X","x1","X")), Right(Pending("X","x2","X"))),
            "x1" -> Seq(Left("X")),
            "x2" -> Seq(Left("X"))
           )
            
      val result = checker("x0","X",ctx)
      println(s"Result: $result")
      result.isOK should be(true)
      result.responses.length should be(1)
      val r1 = result.responses.head
      r1._2 should contain ("x0" -> "X")
      r1._2 should contain ("x1" -> "X")
      val r2 = result.responses.tail.head
      r2._2 should contain ("x0" -> "X")
      r2._2 should contain ("x2" -> "X")
    }
    * 
    * 
    */
  }
  
}


