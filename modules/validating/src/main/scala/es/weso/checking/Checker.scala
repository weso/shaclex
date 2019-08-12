package es.weso.checking

trait Checker {
  type Config
  type Env
  type Err
  type Log
  type Check[A]

  def getConfig: Check[Config]

  def getEnv: Check[Env]

  def addLog(log: Log): Check[Unit]

  //  def logStr(msg: String): Check[Unit]

  def local[A](f: Env => Env)(c: Check[A]): Check[A]

  def ok[A](x: A): Check[A]

  def err[A](e: Err): Check[A]

  def orElse[A](c1: Check[A], c2: => Check[A]): Check[A]

  def checkSome[A](cs: List[Check[A]], errorIfNone: Err): Check[A]

  def attempt[A](c: Check[A]): Check[Either[Err, A]]

  def run[A](c: Check[A])(config: Config)(env: Env): (Log, Either[Err, A])

  def runCheck[A](c: Check[A])(config: Config)(env: Env): (Either[Err, A], Log) =
    run(c)(config)(env).swap

  def runValue[A](c: Check[A])(config: Config)(env: Env): Either[Err, A] =
    run(c)(config)(env)._2

  def runLog[A](c: Check[A])(config: Config)(env: Env): Log =
    run(c)(config)(env)._1

}
