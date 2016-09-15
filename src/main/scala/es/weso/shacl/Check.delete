package es.weso.shacl
import cats._, data._
import cats.implicits._
import es.weso.rdf._

abstract class checker {
  
type Config
type Env 
implicit val envMonoid: Monoid[Env]
type Err 
 
type ReaderEnv[A] = Kleisli[Id,Env,A]
type ReaderConfig[A] = Kleisli[ReaderEnv,Config,A]
type WriterLog[A] = WriterT[ReaderConfig,Logger,A]

type WriterLogAux[F[_],A] = WriterT[F,Logger,A]

type Check[A] = EitherT[WriterLog,ViolationError,A]
type CheckAux[F[_],A] = EitherT[F,ViolationError,A]

// Logger...
case class Logger(log: String) 
  
implicit def loggerMonoid = new Monoid[Logger] {
    def empty = Logger("")
    
    // TODO: Not efficient, just for testing...
    def combine(l1: Logger, l2: Logger): Logger = 
      Logger(l1.log ++ "\n" ++ l2.log)
}
  
implicit def loggerShow = new Show[Logger] {
    def show(l: Logger) = s"Logger: \n${l.log}" 
}



}