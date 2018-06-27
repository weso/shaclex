package es.weso.shex.spec
import cats._
import cats.data.{EitherT, ReaderT}
import cats.implicits._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps.ShapeMapLabel
import es.weso.shex.Schema
import es.weso.shex.btValidator.ShExErr
import es.weso.typing.Typing

object Check {

  type ShapeTyping = Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]]
  type ReaderEnv[A] = ReaderT[Id, Env, A]
  type Check[A] = EitherT[ReaderEnv,String,A]

  def optSatisfy[A](maybeA: Option[A], check: A => Check[Boolean]): Check[Boolean] =
   maybeA match {
    case None => true.pure[Check]
    case Some(a) => check(a)
  }

  def satisfyOr(c1: Check[Boolean],
                c2: => Check[Boolean]
               ): Check[Boolean] = c1.flatMap(x =>
    if (x) pure(true) else c2
  )

  def satisfyAnd(c1: Check[Boolean],
                 c2: => Check[Boolean]
                ): Check[Boolean] = c1.flatMap(x =>
    if (x) c2 else pure(false)
  )

  def satisfyAll(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.forall(_ == true))

  def satisfySome(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.exists(_ == true))

  def satisfyNot(check: Check[Boolean]): Check[Boolean] =
    check.map(e => !e)

  def satisfyFirst[A,F[_]: Monad](ls: => Stream[A],
                                  check: A => F[Boolean]
                                 ): F[Boolean] = {
    val z : Eval[F[Boolean]] = Eval.later(Monad[F].pure(false))
    def cmb(x : A, next: Eval[F[Boolean]]): Eval[F[Boolean]] =
      Eval.later(
        for {
         b <- check(x)
         n <- if (b) Monad[F].pure(true)
              else next.value
        } yield n
      )
    Foldable[Stream].foldRight(ls,z)(cmb).value
  }


  def unimplemented[A](msg: String): Check[A] =
    err(s"Unimplemented: $msg")

  def pure[A](x:A): Check[A] = x.pure[Check]

  def err[A](msg: String): Check[A] = EitherT.leftT[ReaderEnv,A](msg)

  def getSchema: Check[Schema] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.schema
  }

  def getTyping: Check[ShapeTyping] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.typing
  }

  def getRDF: Check[RDFReader] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.rdf
  }

  def runLocal[A](fn: Env => Env, check: Check[A]): Check[A] = {
    EitherT(check.value.local(fn))
  }

  def runLocalWithTyping[A](fn: ShapeTyping => ShapeTyping, check: Check[A]): Check[A] = {
    runLocal(env => env.copy(typing = fn(env.typing)), check)
  }

  def runCheck[A](env: Env, check: Check[A]): Either[String,A] = {
    check.value.run(env)
  }



}