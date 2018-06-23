package es.weso.shex.spec
import cats._
import cats.data.{EitherT, ReaderT}
import cats.implicits._
import es.weso.shex.Schema
import es.weso.shex.btValidator.BtValidator.ReaderEnv

object Check {

  type ReaderSchema[A] = ReaderT[Id,Schema,A]
  type Check[A] = EitherT[ReaderSchema,String,A]

  def optSatisfy[A](maybeA: Option[A], check: A => Check[Boolean]): Check[Boolean] =
   maybeA match {
    case None => true.pure[Check]
    case Some(a) => check(a)
  }

  def satisfyAll(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.forall(_ == true))

  def satisfySome(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.exists(_ == true))

  def satisfyNot(check: Check[Boolean]): Check[Boolean] =
    check.map(e => !e)

  def satisfyFirst[A](ls: Stream[A],
                      check: A => Check[Boolean]
                     ): Check[Boolean] = {
    val z = pure(false)
    def cmb(next: Check[Boolean], x: A): Check[Boolean] = for {
      b <- check(x)
      n <- if (b) pure(true)
           else next
    } yield n
    ls.foldLeft(z)(cmb)
  }


  def unimplemented[A](msg: String): Check[A] =
    err(s"Unimplemented: $msg")

  def pure[A](x:A): Check[A] = x.pure[Check]

  def err[A](msg: String): Check[A] = EitherT.leftT[ReaderSchema,A](msg)

  def getSchema: Check[Schema] = {
    val r: ReaderSchema[Schema] = ReaderT.ask
    EitherT.liftF[ReaderSchema,String,Schema](r)
  }

  def runCheck[A](schema: Schema, check: Check[A]): Either[String,A] =
    check.value.run(schema)


}