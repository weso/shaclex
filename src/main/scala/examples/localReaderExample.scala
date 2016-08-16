package examples
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import cats.implicits._

object ExampleLocalReader {
  
  def exampleInterpreter = {
   type Env = Map[String,Int] 
   type ReaderEnv[A] = Reader[Env, A]
   type Comp = Fx.fx2[ReaderEnv, Option]
  
   def lookup(x: String): Eff[Comp,Int] = for {
     e <- ask[Comp,Env]
     v <- OptionEffect.fromOption[Comp,Int](e.get(x))
   } yield v
  
   def runLocal[A](f:Env => Env, c: Eff[Comp,A]): Eff[Comp,A] = 
     c.modifyReader(f)
   
   def program: Eff[Comp,String] = for {
    v <- runLocal(_.updated("x",2), lookup("x"))
    e <- ask[Comp,Env]
   } yield s"Value: $v, env: $e"
  
   val env: Env = Map()

   program.runReader(env).runOption.run

  }
  
}