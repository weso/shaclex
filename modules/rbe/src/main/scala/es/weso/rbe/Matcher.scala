package es.weso.rbe
import es.weso.typing._
import es.weso.checking._
import cats._
import implicits._

case class MatcherLog[Edge, Node, Label, Evidence](
  messages: List[(Attempt[Node, Label], String)])

case class Attempt[Node, Label](node: Node, label: Label)

trait Matcher[Edge, Node, Label, Evidence] extends CheckerCats {
  type Log = MatcherLog[Edge, Node, Label, Evidence]
  type Attempt_ = Attempt[Node, Label]
  type LogMessage = (Attempt_, String)
  type Schema_ = Schema[Edge, Node, Label, Err, Evidence]
  type Graph_ = Graph[Edge, Node]
  type Config = (Schema_, Graph_)
  type Typing_ = Typing[Node, Label, Err, Evidence]
  type Env = Typing_
  type CheckTyping = Check[Typing_]
  type Triples_ = Set[(Node, Edge, Node)]
  type CheckInfo = Triples_

  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env): Env = e1.combineTyping(e2)
    def empty: Env = Typing.empty
  }

  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log =
      MatcherLog(l1.messages |+| l2.messages)
    def empty: Log = MatcherLog(List())
  }

  implicit val logShow: Show[Log] = new Show[Log] {
    def show(log: Log): String =
      log.messages.map { case (ns, msg) => s"$ns - $msg" }.mkString("\n")
  }

  implicit val typingShow: Show[Env] = new Show[Env] {
    def show(t: Env): String = t.toString
  }

  def matchNodeLabel(
    node: Node,
    label: Label): Check[Typing[Node, Label, Err, Evidence]]

  def getTyping: Check[Typing[Node, Label, Err, Evidence]] = getEnv

  def getSchema: Check[Schema_] = for {
    c <- getConfig
  } yield c._1

  def getGraph: Check[Graph_] = for {
    c <- getConfig
  } yield c._2

  /* def run[A](schema: Schema_, graph: Graph_, check: Check[A]): (Log,(Either[Err,A],CheckInfo)) = {
  val e0: Env = Typing.empty
  val c0: Config = (schema,graph)
  val o0: CheckInfo = Set()
  runCheck(check)(c0)(e0)(o0)
 } */

  /**
   * Runs a checker and discards log and checkInfo
   */
  /*def runValue[A](schema: Schema_, graph: Graph_, check: Check[A]): Either[Err,A] =
   run(schema,graph,check)._2._1 */

}