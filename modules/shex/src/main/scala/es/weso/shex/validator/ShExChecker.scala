package es.weso.shex.validator

import cats._
import data._
import implicits._
import es.weso.checking.CheckerCats
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{ BNodeId, IRI, Literal, RDFNode }
import es.weso.shex.{ implicits => _, _ }

object ShExChecker extends CheckerCats {

  import ShapeTyping._

  type Config = RDFReader
  type Env = ShapeTyping
  type Err = ViolationError
  type Evidence = (NodeShape, String)
  type Log = List[Evidence]
  type CheckTyping = Check[ShapeTyping]

  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env): Env = e1 |+| e2
    def empty: Env = Monoid[ShapeTyping].empty
  }
  /*    implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
        def log(msg: String): Log =
          throw new Exception("Not implemented logCanlog")
      } */
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map { case (ns, msg) => s"${ns}: $msg" }.mkString("\n")
  }
  implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
    def show(t: ShapeTyping): String = t.toString
  }

  def errStr[A](msg: String): Check[A] =
    err[A](ViolationError.msgErr(msg))

  def checkCond(
    condition: Boolean,
    attempt: Attempt,
    error: ViolationError,
    evidence: String): CheckTyping = for {
    _ <- validateCheck(condition, error)
    newTyping <- addEvidence(attempt.nodeShape, evidence)
  } yield newTyping

  def addEvidence(nodeShape: NodeShape, msg: String): Check[ShapeTyping] = {
    for {
      t <- getTyping
      _ <- addLog(List((nodeShape, msg)))
    } yield t.addEvidence(nodeShape.node, nodeShape.shape, msg)
  }

  def addNotEvidence(nodeShape: NodeShape, e: ViolationError, msg: String): Check[ShapeTyping] = {
    val node = nodeShape.node
    val shape = nodeShape.shape
    for {
      t <- getTyping
      _ <- addLog(List((nodeShape, msg)))
    } yield t.addNotEvidence(node, shape, e)
  }

  def runLocal[A](
    c: Check[A],
    f: ShapeTyping => ShapeTyping): Check[A] =
    local(f)(c)

  def runLocalSafe[A](
    c: Check[ShapeTyping],
    f: ShapeTyping => ShapeTyping,
    safe: (Err, ShapeTyping) => ShapeTyping): Check[ShapeTyping] =
    cond(local(f)(c), (t: ShapeTyping) => ok(t), err => for {
      t <- getTyping
    } yield safe(err, t))

  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = getEnv // ask[Comput,ShapeTyping]

  def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }

  def runCheck[A: Show](
    c: Check[A],
    rdf: RDFReader): CheckResult[ViolationError, A, Log] = {
    val initial: ShapeTyping = Monoid[ShapeTyping].empty
    runCheckWithTyping(c, rdf, initial)
  }

  def runCheckWithTyping[A: Show](
    c: Check[A],
    rdf: RDFReader,
    typing: ShapeTyping): CheckResult[ViolationError, A, Log] = {
    val r = run(c)(rdf)(typing)
    CheckResult(r)
  }

}
