package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.checking.CheckerCats
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI
import es.weso.shex.ShExError
import es.weso.shex.validator.Action._

object ShExChecker2 extends CheckerCats {

  import ShapeTyping._

  type Config = RDFReader
  type Env = (ShapeTyping, VarTable)
  type Err = ShExError
  type Evidence = (NodeShape, String)
  type Log = List[Action]
  type CheckTyping = Check[ShapeTyping]

  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env): Env = e1 |+| e2
    def empty: Env = Monoid[(ShapeTyping,VarTable)].empty
  }
  /*    implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
        def log(msg: String): Log =
          throw new Exception("Not implemented logCanlog")
      } */
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
/*  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map { case (ns, msg) => s"${ns}: $msg" }.mkString("\n")
  } */
  implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
    def show(t: ShapeTyping): String = t.toString
  }

  def errStr[A](msg: String): Check[A] =
    err[A](ShExError.msgErr(msg))

  def checkCond(
                 condition: Boolean,
                 attempt: Attempt,
                 error: ShExError,
                 evidence: String): CheckTyping = for {
    _ <- validateCheck(condition, error)
    newTyping <- addEvidence(attempt.nodeShape, evidence)
  } yield newTyping

  def addEvidence(nodeShape: NodeShape, msg: String): Check[ShapeTyping] = {
    val action = Action(IRI("http://shex.io/actions/log"),Some(s"Evidence added: $nodeShape: $msg"))
    for {
      t <- getTyping
      _ <- addLog(List(action))
    } yield t.addEvidence(nodeShape.node, nodeShape.shape, msg)
  }


  def addNotEvidence(nodeShape: NodeShape, e: ShExError, msg: String): Check[ShapeTyping] = {
    val action = Action(IRI("http://shex.io/actions/log"),Some(s"Not Evidence: $nodeShape: $msg"))
    val node = nodeShape.node
    val shape = nodeShape.shape
    for {
      t <- getTyping
      _ <- addLog(List(action))
    } yield t.addNotEvidence(node, shape, e)
  }

  def runLocal[A](
    c: Check[A],
    f: Env => Env): Check[A] =
    local(f)(c)

  def runLocalSafe[A](
    c: Check[Env],
    f: Env => Env,
    safe: (Err, Env) => Env): Check[Env] = {
    def fnOk(t: Env): Check[Env] = ok(t)
    def fnErr(err:Err): Check[Env] = for {
      t <- getEnv
    } yield safe(err, t)

    cond(local(f)(c), fnOk, fnErr)
  }

  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = for {
    env <- getEnv
  } yield env._1 // ask[Comput,ShapeTyping]

  def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }

  def runCheck[A: Show](
    c: Check[A],
    rdf: RDFReader): CheckResult[ShExError, A, Log] = {
    val initial: (ShapeTyping,VarTable) = Monoid[(ShapeTyping,VarTable)].empty
    runCheckWithTyping(c, rdf, initial)
  }

  def runCheckWithTyping[A: Show](
    c: Check[A],
    rdf: RDFReader,
    typing: (ShapeTyping,VarTable)): CheckResult[ShExError, A, Log] = {
    val r = run(c)(rdf)(typing)
    CheckResult(r)
  }

}
