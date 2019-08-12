package es.weso.shacl.validator

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.checking.CheckerCats
import es.weso.shacl.report.AbstractResult
// import es.weso.utils.MyLogging

object SHACLChecker extends CheckerCats with LazyLogging {

  type Config = RDFReader
  type Env = ShapeTyping
  type Err = AbstractResult
  type Log = List[Evidence]
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map(_.show).mkString("\n")
  }

  private def combineEnv(t1: Env, t2: Env): Env =
    Monoid[Env].combine(t1,t2)

  private[validator] def combineResults(x: Result, y: Result): Result = {
    val z = combineEnv(x._1,y._1)
    (z, x._2 && y._2)
  }

  private[validator] def checkAllWithTyping[A](cs: LazyList[A], chk: A => CheckTyping): CheckTyping = for {
    t <- getTyping
    r <- checkAllFlag(cs, chk, t)
  } yield r

  private[validator] def done: CheckTyping = for {
    t <- getTyping
  } yield (t,true)

  private[validator] def fail(msg: String): CheckTyping = {
    logger.debug(s"Failed: $msg")
    for {
      t <- getTyping
    } yield (t,false)
  }


  private[validator] def combineResultSeq(ts: Seq[Result]): CheckTyping = {
    val zero: (ShapeTyping,Boolean) = (ShapeTyping.empty, true)
    def cmb(x: Result, y: Result): Result = {
      (x._1 |+| y._1, x._2 && y._2)
    }
    ok(ts.foldRight(zero)(cmb))
  }

  private[validator] def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }

  private[validator] def getRDF: Check[RDFReader] = getConfig

  private[validator] def getTyping: Check[ShapeTyping] = getEnv

  private[validator] def addLogMsg(msg: String): Check[Unit] =
    addLog(List(MsgEvidence(msg)))


  private[validator] def runLocalTyping[A](c: Check[A], f: ShapeTyping => ShapeTyping): Check[A] =
    local(f)(c)

  private[validator] def checkAllTyping[A](ls: LazyList[A], chk: A => CheckTyping): CheckTyping = for {
    t <- getTyping
    r <- checkAllFlag(ls, chk, t)
  } yield r

  private[validator] def checkSequenceTyping(ls: List[CheckTyping]): CheckTyping = for {
    t <- getTyping
    r <- checkSequenceFlag(ls,t)
  } yield r

}
