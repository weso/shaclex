package es.weso.checking
import cats._, data._
import cats.implicits._

abstract class CheckerCats extends Checker {
//  import CanLog.ops._
  implicit val envMonoid: Monoid[Env]
//  implicit val logCanLog: CanLog[Log]
  implicit val logMonoid: Monoid[Log]

  type ReaderConfig[A] = Kleisli[Id, Config, A]
  type ReaderEC[A] = Kleisli[ReaderConfig, Env, A]
  type WriterEC[A] = WriterT[ReaderEC, Log, A]
  type Check[A] = EitherT[WriterEC, Err, A]

  def getConfig: Check[Config] = {
    readerConfig2check(Kleisli.ask[Id, Config])
  }

  def getEnv: Check[Env] = {
    readerEC2check(Kleisli.ask[ReaderConfig, Env])
  }

  def addLog(log: Log): Check[Unit] = {
    writerEC2check(WriterT.tell[ReaderEC, Log](log))
  }

/*  def logStr(msg: String): Check[Unit] = {
    addLog(CanLog[Log].log(msg))
  } */

  def local[A](f: Env => Env)(c: Check[A]): Check[A] = {
    EitherT(runLocalW(f)(c.value))
  }

  def ok[A](x: A): Check[A] =
    EitherT.pure[WriterEC, Err, A](x)

  def err[A](e: Err): Check[A] =
    EitherT.left[WriterEC, Err, A](mkErr[WriterEC](e))

  def orElse[A](c1: Check[A], c2: => Check[A]): Check[A] =
    c1.orElse(c2)

  def checkSome[A](cs: List[Check[A]], errorIfNone: Err): Check[A] = {
    lazy val z: Check[A] = err(errorIfNone)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  def checkSome[A](cs: List[Check[A]])
                  (implicit ev: Monoid[Err]): Check[A] = {
    lazy val z: Check[A] = err(ev.empty)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  /**
    * Given a list of checks, return the list of values that pass
    */
  def checkLs[A](cs: List[Check[A]]): Check[List[A]] = {
    lazy val z: Check[List[A]] = ok(List())
    val css : List[Check[List[A]]] = cs.map(c => c.map(List(_)).orElse(z))
    def comb(rest: Check[List[A]], current: Check[List[A]]): Check[List[A]] = for {
      xs <- rest
      ys <- current
    } yield (xs ++ ys)
    css.foldLeft(z)(comb)
  }

  def checkOneOf[A](cs: List[Check[A]], errNone: Err, errMoreThanOne: List[A] => Err): Check[A] = for {
    rs <- checkLs(cs)
    v <- rs.length match {
      case 0 => err[A](errNone)
      case 1 => ok(rs.head)
      case _ => err[A](errMoreThanOne(rs))
    }
  } yield v


  def attempt[A](c: Check[A]): Check[Either[Err, A]] = for {
    v <- MonadError[Check, Err].attempt(c)
  } yield v

  /**
    * Returns the list of values whose computation is successful
    * @param ls list of values
    * @param check computation to check for each value
    * @tparam A type of values
    * @tparam B type returned by computation
    * @return a computation with a list of pairs for whom the computation was successful
    */
  def filterSuccess[A,B](ls: List[A], check: A => Check[B]): Check[List[(A,B)]] = {
    val zero: Check[List[(A,B)]] = ok(List())
    def comb(rest: Check[List[(A,B)]], current: A): Check[List[(A,B)]] = for {
      rs <- rest
      c <- attempt(check(current))
    } yield c match {
      case Left(err) => rs
      case Right(b) => (current,b) +: rs
    }
    ls.foldLeft(zero)(comb)
  }

  def cond[A,B](
    c: Check[A],
    thenPart: A => Check[B],
    elsePart: Err => Check[B]): Check[B] =
   attempt(c).flatMap(_.fold(elsePart(_),thenPart(_)))

  def checkList[A,B](ls: List[A], check: A => Check[B]): Check[List[B]] = {
    checkAll(ls.map(check))
  }

  /**
  * Checks all elements in a list
  * If any of the elements fail, fails
  */
  def checkAll[A](xs: List[Check[A]]): Check[List[A]] = xs.sequence

  def checkPair1st[A,B](p: (Check[A],B)): Check[(A,B)] = for {
    v <- p._1
  } yield (v,p._2)

  def checkPair2nd[A,B](p: (A,Check[B])): Check[(A,B)] = for {
    v <- p._2
  } yield (p._1,v)

  /*  def optCheck[A,B](
    x: Option[A],
    f: => A => Check[B]): Check[Option[B]] = x match {
    case None => ok(None)
    case Some(v) => f(v).map(Some(_))
  } */

   def optCheck[A,B](
     c: Option[A],
     check: A => Check[B],
     default: => Check[B]
  ): Check[B] = c.fold(default)(check(_))


  def validateCheck(condition: Boolean, e: Err): Check[Unit] = {
    if (condition) Monad[Check].pure(())
    else err(e)
  }

  //implicit val monadCheck = implicitly[Monad[Check]]
  lazy val mWriterEC = implicitly[Monad[WriterEC]]

  def run[A](c: Check[A])(config: Config)(env: Env): (Log, Either[Err, A]) =
    c.value.run.run(env).run(config)

  def mkErr[F[_]: Applicative](e: Err): F[Err] =
    Applicative[F].pure(e)

  // Utility
  def runLocalW[A](f: Env => Env)(c: WriterEC[A]): WriterEC[A] = {
    val cr: ReaderEC[(Log, A)] = c.run
    val x: ReaderEC[(Log, A)] = cr.local(f)
    val r: WriterEC[(Log, A)] = readerEC2writer(x)
    r.mapBoth { case (_, (w, x)) => (w, x) }
  }

  def readerConfig2check[A](c: ReaderConfig[A]): Check[A] =
    readerEC2check(c.liftT[λ[(F[_], A) => Kleisli[F, Env, A]]])

  def readerEC2writer[A](c: ReaderEC[A]): WriterEC[A] =
    // c.liftT[λ[(F[_], A) => WriterT[F, Log, A]]]
    WriterT.lift[ReaderEC,Log,A](c)

  def readerEC2check[A](c: ReaderEC[A]): Check[A] =
    // writerEC2check(c.liftT[λ[(F[_], A) => WriterT[F, Log, A]]])
    writerEC2check(readerEC2writer(c))

  def writerEC2check[A](c: WriterEC[A]): Check[A] =
     // c.liftT[λ[(F[_], A) => EitherT[F, Err, A]]]
     EitherT.liftT(c)

}

// Example implementation for testing purposes
object CheckerCatsStr extends CheckerCats {
  type Err = String
  type Config = Map[String, String]
  type Env = Map[String, Int]
  type Log = List[String]

  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env) = e1 |+| e2
    def empty = Map()
  }
/*  implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
    def log(str: String) = List(str)
  } */
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log) = l1 ++ l2
    def empty = List()
  }

  // I need to add the following line so Check can act as a Monad...
  implicit val monadWriter = mWriterEC

  def c0: Config = Map[String, String]()
  def e0: Env = Map[String, Int]()
  def run0[A](c: Check[A]): (Log, Either[Err, A]) =
    run(c)(c0)(e0)

  def c1: Check[Int] = logStr("one") >> ok(1)
  def c2: Check[Int] = logStr("two") >> ok(2)
  def e: Check[Int] = logStr("err") >> err("Err")

  def logStr(msg: String): Check[Unit] = {
    addLog(List(msg))
  }
}

