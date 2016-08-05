package es.weso.validator
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

trait Checked {
  type S // State 
  type Err // Errors
  type Ctx // Context

  // With this import we use list for non determinism
  // We could import Option if we are only interested in one answer
  import cats.std.list._

  type Result[A] =  Xor[NonEmptyList[Err],List[(A,S)]]
  
  def isOK[A](r: Result[A]): Boolean = 
    r.isRight && r.toList.isEmpty == false  
  
  // Computational effects
  type Comput =  
    Reader[Ctx,?] |:  
    State[S,?] |:
    Choose |: 
    Validate[Err, ?] |:
    Eval |:
    NoEffect

 type Check[A] = Eff[Comput,A]

 def runner[A](ctx: Ctx, state: S, c: Check[A]): Result[A] =
   c.runReader(ctx).runState(state).runChoose.runNel.runEval.run
 
 
}