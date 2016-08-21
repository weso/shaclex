package examples
import cats._, data._
import cats.syntax.all._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._


object Backtracking0 {
  
//// With 3 effects, it fails when I use runNel at the beginning

type C = Fx.fx3[
  Writer[String,?],
  Reader[String,?],
  Validate[String,?]
  ]

type Check[A] = Eff[C,A]

val v3 : Check[Int] = pure(3)
val v4 : Check[Int] = pure(4)
val w : Check[Int] = wrong[C,String]("Wrong value") >> pure(0) 

// The following code fails
val p0 : Check[Int] = catchWrong[C,String,Int](w)(e => pure(4))


val p0r : Check[Int] = w.catchWrong[String](e => pure(4))

def checkSome[A](xs: List[Check[A]], v: A): Check[A] = {
  val z: Check[A] = wrong[C,String]("No one") >> pure(v)
  def comb(x:Check[A], y:Check[A]): Check[A] = {
    x.catchWrong[String](e => y)
  }
  xs.foldLeft(z)(comb)
}

val p1: Check[Int] = checkSome(List(w,v3),0)
val p2: Check[Int] = checkSome(List(v3,w),0)
val p3: Check[Int] = checkSome(List(w,w),0)

def r[A](c: Check[A]) = c.runNel.runReader("Hi").runWriter.run

}

object Backtracking1 {
  
import ErrorEffect.{ok => OK, ErrorOrOk}

type C = Fx.fx3[Writer[String,?],Reader[String,?],ErrorOrOk]

type Check[A] = Eff[C,A]

val v3 : Check[Int] = for {
  _ <- tell[C,String]("v3") 
} yield 3

val v4 : Check[Int] = for {
  _ <- tell[C,String]("v4") 
} yield 4

val w : Check[Int] = for {
  _ <- tell[C,String]("w")
  v <- ErrorEffect.fail[C,Int]("Wrong value")
} yield v  

// The following code fails
val p0 : Check[Int] = w orElse v3


def checkSome[A](xs: List[Check[A]]): Check[A] = {
  val z: Check[A] = ErrorEffect.fail[C,A]("No one") 
  def comb(x:Check[A], y:Check[A]): Check[A] = {
    x orElse y
  }
  xs.foldRight(z)(comb)
}

val p1: Check[Int] = checkSome(List(w,v3))
val p2: Check[Int] = checkSome(List(v3,w))
val p3: Check[Int] = checkSome(List(w,w))

def r[A](c: Check[A]) = c.runError.runReader("Hi").runWriter.run

}


object Backtracking2 {
  
import ErrorEffect.{ok => OK, ErrorOrOk}

//// With 3 effects, it fails when I use runNel at the beginning


case class MyError(msg: String)
object MyErrorEffect extends ErrorEffect[MyError]

type MyErrorOrOk[A] = MyErrorEffect.ErrorOrOk[A]
type C = Fx.fx3[Writer[String,?],Reader[String,?],MyErrorOrOk]

type Check[A] = Eff[C,A]

val v3 : Check[Int] = for {
  _ <- tell[C,String]("v3") 
} yield 3

val v4 : Check[Int] = for {
  _ <- tell[C,String]("v4") 
} yield 4

val w : Check[Int] = for {
  _ <- tell[C,String]("w")
  v <- MyErrorEffect.fail[C,Int](MyError("Wrong value"))
} yield v  

// The following code fails
// val p0 : Check[Int] = w.orElse


/*def checkSome[A](xs: List[Check[A]]): Check[A] = {
  val z: Check[A] = ErrorEffect.fail[C,A]("No one") 
  def comb(x:Check[A], y:Check[A]): Check[A] = {
    x orElse y
  }
  xs.foldRight(z)(comb)
}

val p1: Check[Int] = checkSome(List(w,v3))
val p2: Check[Int] = checkSome(List(v3,w))
val p3: Check[Int] = checkSome(List(w,w))

def r[A](c: Check[A]) = c.runError.runReader("Hi").runWriter.run
*/
}