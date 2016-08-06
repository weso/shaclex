package tests
import org.scalatest._
import cats._, data._

import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

class EffTest extends FunSpec with Matchers with TryValues with OptionValues {
 
 describe("Testing catchWrong...") {
    
  type Typing = Map[String,Int]
  type ViolationError = String
  type Comput = Fx.fx4[State[Typing,?], Validate[ViolationError, ?], Choose, Eval]
      
  type Check[A] = Eff[Comput,A]

  import cats.std.list._

  type Result[A] = 
    Xor[NonEmptyList[ViolationError],List[(A,Typing)]]

  val emptyTyping : Typing = Map()
  
  def runCheck[A](c: Check[A]): Result[A] = 
       c.runState(emptyTyping).runChoose.runNel.runEval.run

  it("Can run a single computation...") {
     val comp1: Check[Int] = for {
       _ <- validateCheck[Comput,ViolationError](false,"Error2")
       _ <- validateCheck[Comput,ViolationError](false,"Error3")
     } yield 4
     
     val expected = Xor.Left(NonEmptyList("Error2", "Error3"))
     
     val current = runCheck(comp1)
     current should be(expected)
   }
   
/*  it("Can catch a wrong value in a single computation...") {
    val comp1: Check[Unit] = wrong[Comput,ViolationError]("Error1")
    val handle: ViolationError => Check[Unit] = e => pure(()) 
    val comp2: Check[Unit] = catchWrong(comp1)(handle)
    val expected = Xor.Right(List(((),Map()))) 
    val current = runCheck(comp2)
    println("Current: " + current)
    current should be(expected)
   }

   it("Can catch several wrong values in a single computation...") {
    val comp1: Check[Int] = for {
      _ <- wrong[Comput,ViolationError]("1")
      _ <- wrong[Comput,ViolationError]("2")
      _ <- wrong[Comput,ViolationError]("3")
    } yield 0
    val handle: ViolationError => Check[Int] = { case e => pure(e.toInt) } 
    val comp2: Check[Int] = catchWrong(comp1)(handle)
    val expected = Xor.Right(List(((1),Map()))) 
    val current = runCheck(comp2)
    println("Current: " + current)
    current should be(expected)
   }
*/
 }
 
}