package examples
import cats._, data._
import cats.implicits._

object BacktrackingCats {

  type ViolationError = String
  type ShapeTyping = Map[String,Int]
  type RDF = String
  type Evidence = String
  
  type Check[A] = ReaderT[Validated[ViolationError,?],RDF,A]
  
/*  val p: Check[Int] = for {
    rdf <- ReaderT[Validated[ViolationError,?].ask
  } yield 3 */ 

}

object ExampleMonadReader {
  
  type R[A] = Reader[String, A]
  val r = MonadReader[R, String]
  
  val f: R[(String,Int)] = for {
     s <- r.ask
     a <- r.pure(3)
  } yield (s + "bar", a + 1)
  
  val r0 = f.run("Hola")
}

object ExampleMonadWriter {
  
  type W[A] = Writer[String, A]
  val w = MonadWriter[W, String]
  
  val f = for {
     _ <- w.tell("Hola")
     _ <- w.tell("Pepe")
     a <- w.pure(3)
  } yield 2
  
  val r0 = f.run
}

