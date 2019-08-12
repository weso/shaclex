package es.weso.checking
import cats._
import data._
import cats.implicits._

abstract class CheckerCats extends Checker {
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

  def local[A](f: Env => Env)(c: Check[A]): Check[A] = {
    EitherT(runLocalW(f)(c.value))
  }

  def ok[A](x: A): Check[A] =
    EitherT.pure[WriterEC, Err](x)

  def err[A](e: Err): Check[A] =
    EitherT.left[A](mkErr[WriterEC](e))

  def fromEither[A](e: Either[Err,A]): Check[A] = EitherT.fromEither[WriterEC](e)


  def orElse[A](c1: Check[A], c2: => Check[A]): Check[A] =
    c1.orElse(c2)

  def checkSome[A](cs: List[Check[A]], errorIfNone: Err): Check[A] = {
    lazy val z: Check[A] = err(errorIfNone)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  def checkSome[A](cs: LazyList[Check[A]])(implicit ev: Monoid[Err]): Check[A] = {
    lazy val z: Check[A] = err(ev.empty)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  /**
    * Given a computation check that returns a pair of value and a flag, returns the first value whose flag is true
    * If none is true, returns the value of the computation parameter last
    * @param ls
    * @param check
    * @param last
    * @tparam A
    * @tparam B
    * @tparam F
    * @return
    */
  def checkSomeFlag[A,B,F[_]: Monad](ls: => LazyList[A],
                                check: A => F[(B,Boolean)],
                                last: F[(B,Boolean)]
                               ): F[(B,Boolean)] = {
    val z : Eval[F[(B,Boolean)]] = Eval.later(last)
    def cmb(x : A, next: Eval[F[(B,Boolean)]]): Eval[F[(B,Boolean)]] =
      Eval.later(
        for {
          r <- check(x)
          n <- if (r._2) Monad[F].pure(r)
          else next.value
        } yield n
      )
    Foldable[LazyList].foldRight(ls,z)(cmb).value
  }

  def checkSomeFlagCount[A,B: Monoid](ls: => LazyList[A],
                                          check: A => Check[(B,Boolean)],
                                          last: B
                                    ): Check[(B,Int)] = {
    val z : Eval[Check[(B,Int)]] = Eval.later(ok((last,0)))
    def cmb(x : A, next: Eval[Check[(B,Int)]]): Eval[Check[(B,Int)]] =
      Eval.later(
        for {
          r1 <- check(x)
          r2 <- next.value
        } yield {
          if (r1._2) (r1._1 |+| r2._1, 1 + r2._2)
          else r2
        }
      )
    Foldable[LazyList].foldRight(ls,z)(cmb).value
  }


  def checkAllFlag[A,B: Monoid, F[_]: Monad](ls: => LazyList[A],
                                             check: A => F[(B,Boolean)],
                                             last: => B
                                            ): F[(B,Boolean)] = {
    val z : Eval[F[(B,Boolean)]] = {
      Eval.later(Monad[F].pure((last,true)))
    }
    def cmb(x : A, next: Eval[F[(B,Boolean)]]): Eval[F[(B,Boolean)]] = {
      Eval.later(
        for {
          r <- check(x)
          n <- next.value
        } yield {
          val newR = (n._1 |+| r._1, n._2 && r._2)
          newR
        }
      )
    }
    Foldable[LazyList].foldRight(ls,z)(cmb).value
  }


  def checkAllFailFAtFirstFlag[A,B: Monoid, F[_]: Monad](ls: => LazyList[A],
                                             check: A => F[(B,Boolean)],
                                             last: => B
                                            ): F[(B,Boolean)] = {
    val z : Eval[F[(B,Boolean)]] = {
      Eval.later(Monad[F].pure((last,true)))
    }
    def cmb(x : A, next: Eval[F[(B,Boolean)]]): Eval[F[(B,Boolean)]] = {
      Eval.later(
        for {
          r <- check(x)
          n <- if (!r._2) Monad[F].pure(r)
          else for {
            v <- next.value
          } yield (v._1 |+| r._1, v._2)
        } yield n
      )
    }
    Foldable[LazyList].foldRight(ls,z)(cmb).value
  }

  def checkSequenceFlag[A: Monoid, F[_]: Monad](ls: => List[F[(A,Boolean)]],
                                                last: A): F[(A,Boolean)] = {
    val z : Eval[F[(A,Boolean)]] = Eval.later(Monad[F].pure((last,true)))
    def cmb(x : F[(A,Boolean)], next: Eval[F[(A,Boolean)]]): Eval[F[(A,Boolean)]] =
      Eval.later(for {
          r1 <- x
          r2 <-next.value
      } yield (r1._1 |+| r2._1, r1._2 && r2._2)
    )
    Foldable[List].foldRight(ls,z)(cmb).value
  }

  /**
    * Run a computation in a local environment. If the computation fails, return the result of calling `safe` function over the current environment
    * @param c computation to run
    * @param f environment
    * @param safe function to call if the computation fails
    * @tparam A
    * @return
    */
  def runLocalSafe[A](c: Check[A],
                      f: Env => Env,
                      safe: (Err, Env) => A
                     ): Check[A] = {
    def fnOk(t: A): Check[A] = ok(t)
    def fnErr(err:Err): Check[A] = for {
      t <- getEnv
    } yield safe(err, t)
    cond(local(f)(c), fnOk, fnErr)
  }

  def runLocal[A](c: Check[A],
                  f: Env => Env): Check[A] =
    local(f)(c)


  /**
   * Given a list of checks, return the list of values that pass
   * It never fails (in case of failure, it ignores the value)
   */
  def checkLs[A](cs: List[Check[A]]): Check[List[A]] = {
    lazy val z: Check[List[A]] = ok(List())
    val css: List[Check[List[A]]] = cs.map(c => c.map(List(_)).orElse(z))
    def comb(rest: Check[List[A]], current: Check[List[A]]): Check[List[A]] = for {
      xs <- rest
      ys <- current
    } yield xs ++ ys
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
  def filterSuccess[A, B](ls: List[A], check: A => Check[B]): Check[List[(A, B)]] = {
    val zero: Check[List[(A, B)]] = ok(List())
    def comb(rest: Check[List[(A, B)]], current: A): Check[List[(A, B)]] = for {
      rs <- rest
      c <- attempt(check(current))
    } yield c match {
      case Left(_) => rs
      case Right(b) => (current, b) +: rs
    }
    ls.foldLeft(zero)(comb)
  }

  /** Attempts to execute a check
    * If it fails, applies `thenPart` to the result, otherwise applies `elsePart` to the error
    *
    * @param check Computation to check
    * @param thenPart part to be executed when it passes
    * @param elsePart part to be executed when the check fails
    * @tparam A type returned by the computation
    * @tparam B type returned the the condition
    * @return
    */
  def cond[A, B](
    check: Check[A],
    thenPart: A => Check[B],
    elsePart: Err => Check[B]): Check[B] =
    attempt(check).flatMap(_.fold(elsePart(_), thenPart(_)))

  def condFlag[A, B](
                  check: Check[A],
                  thenPart: A => Check[B],
                  elsePart: Err => Check[B]): Check[(B,Boolean)] =
    attempt(check).flatMap(_.fold(
      elsePart(_).map((_,false)),
      thenPart(_).map((_,true)))
    )


  def checkList[A, B](ls: List[A], check: A => Check[B]): Check[List[B]] = {
    checkAll(ls.map(check))
  }

  /**
   * Checks all elements in a list
   * If any of the elements fail, fails
   */
  def checkAll[A](xs: List[Check[A]]): Check[List[A]] =
    sequence(xs)  // Question: Is this stack safe?

  def sequence[A](xs: List[Check[A]]): Check[List[A]] =
    xs.sequence[Check,A]  // Question: Is this stack safe?


  def checkPair1st[A, B](p: (Check[A], B)): Check[(A, B)] = for {
    v <- p._1
  } yield (v, p._2)

  def checkPair2nd[A, B](p: (A, Check[B])): Check[(A, B)] = for {
    v <- p._2
  } yield (p._1, v)

  /*  def optCheck[A,B](
    x: Option[A],
    f: => A => Check[B]): Check[Option[B]] = x match {
    case None => ok(None)
    case Some(v) => f(v).map(Some(_))
  } */

  /**
    * If `c` is some value, applies `check`, otherwise applies `default`
    * @param c Optional value
    * @param check check function
    * @param default value in case there is no option
    * @tparam A
    * @tparam B
    * @return
    */
  def optCheck[A, B](
    c: Option[A],
    check: A => Check[B],
    default: => Check[B]): Check[B] = c.fold(default)(check(_))

  def validateCheck(condition: Boolean, e: Err): Check[Unit] = {
    if (condition) Monad[Check].pure(())
    else err(e)
  }

  //implicit val monadCheck = implicitly[Monad[Check]]
  protected lazy val mWriterEC = implicitly[Monad[WriterEC]]

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

  def readerConfig2readerEC[A](c: ReaderConfig[A]): ReaderEC[A] = Kleisli.liftF[ReaderConfig, Env, A](c)

  def readerConfig2check[A](c: ReaderConfig[A]): Check[A] = {
    val readerEC: ReaderEC[A] = readerConfig2readerEC(c)
    readerEC2check(readerEC)
  }

  // readerEC2check(c.liftT[λ[(F[_], A) => Kleisli[F, Env, A]]])

  def readerEC2writer[A](c: ReaderEC[A]): WriterEC[A] =
    // c.liftT[λ[(F[_], A) => WriterT[F, Log, A]]]
    WriterT.liftF[ReaderEC, Log, A](c)

  def readerEC2check[A](c: ReaderEC[A]): Check[A] =
    // writerEC2check(c.liftT[λ[(F[_], A) => WriterT[F, Log, A]]])
    writerEC2check(readerEC2writer(c))

  def writerEC2check[A](c: WriterEC[A]): Check[A] =
    // c.liftT[λ[(F[_], A) => EitherT[F, Err, A]]]
    EitherT.liftF(c)

}

// Example implementation for testing purposes
object CheckerCatsStr extends CheckerCats {
  type Err = String
  type Config = Map[String, String]
  type Env = Map[String, Int]
  type Log = List[String]

  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env) = {
      // I would like to use:
      // e1 |+| e2
      // But it gives an implicit error...
      val e: Env = e1 ++ e2
      e
    }
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

  def c1: Check[Int] = logStr("one") *> ok(1)
  def c2: Check[Int] = logStr("two") *> ok(2)
  def e: Check[Int] = logStr("err") *> err("Err")

  def logStr(msg: String): Check[Unit] = {
    addLog(List(msg))
  }
}

