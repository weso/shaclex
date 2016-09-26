package es.weso.shex.validator
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._
// import util.matching._
import es.weso.shex.implicits.showShEx._
import es.weso.mytyping._

/**
 * ShEx validator
 */
case class Validator(schema: Schema) {

  import es.weso.checking._
  import Validator._

  object MyChecker extends Checker {

    type Config = RDFReader
    type Env = ShapeTyping
    type Err = ViolationError
    type Evidence = (NodeShape, String)
    type Log = List[Evidence]

    import ShapeTyping._
    implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
      def combine(e1: Env, e2: Env): Env = e1 |+| e2
      def empty: Env = Monoid[ShapeTyping].empty
    }
    implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
      def log(msg: String): Log =
        throw new Exception("Not implemented logCanlog")
    }
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
  }
  type ShapeChecker = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping

  import MyChecker._

  type CheckTyping = Check[ShapeTyping]
  type NodeChecker = Attempt => RDFNode => CheckTyping

  def checkNodeLabel(node: RDFNode, label: ShapeLabel): CheckTyping =
    for {
      typing <- getTyping
      newTyping <- if (typing.getOkValues(node).contains(label))
        ok(typing)
      else if (typing.getFailedValues(node).contains(label)) {
        errStr[ShapeTyping](s"Failed because $node doesn't match shape $label")
      } else schema.getShape(label) match {
        case None =>
          errStr[ShapeTyping](s"Can't find shape $label is Schema:\n${schema.show}")
        case Some(shape) =>
          runLocal(checkNodeShapeExpr(node, shape), _.addType(node, label))
      }
    } yield newTyping

  def errStr[A](msg: String): Check[A] =
    err[A](ViolationError.strError(msg))

  def checkNodeShapeExpr(node: RDFNode, s: ShapeExpr): CheckTyping = s match{
    case ShapeOr(ss) => throw new Exception("Not implemented ShapeOr")
    case ShapeAnd(ss) => throw new Exception("Not implemented ShapeAnd")
    case ShapeNot(s) => throw new Exception("Not implemented ShapeNot")
    case nc: NodeConstraint => checkNodeConstraint(node,nc)
    case s: Shape => throw new Exception("Not implemented Shape")
    case s: ShapeRef => throw new Exception("Not implemented ShapeRef")
    case s: ShapeExternal => throw new Exception("Not implemented ShapeExternal")
  }

  def checkNodeConstraint(node: RDFNode, s: NodeConstraint): CheckTyping =
  for {
    _ <- getTyping
    t1 <- optCheck(s.nodeKind, checkNodeKind(node), getTyping)
  } yield t1

  def optCheck[A,B](c: Option[A],
                    check: A => Check[B],
                    default: => Check[B]
  ): Check[B] = ???

  def checkNodeKind(node: RDFNode)(nk: NodeKind): CheckTyping = ???

  def runLocal[A](c: Check[A], f: ShapeTyping => ShapeTyping): Check[A] =
    local(f)(c)

  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = getEnv // ask[Comput,ShapeTyping]

  def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }

  def runCheck[A: Show](
    c: Check[A],
    rdf: RDFReader
  ): CheckResult[ViolationError, A, Log] = {
    val initial: ShapeTyping = Monoid[ShapeTyping].empty
    val r = run(c)(rdf)(initial)
    CheckResult(r)
  }

  implicit lazy val showRDFNode = new Show[RDFNode] {
    override def show(n: RDFNode): String = {
      n match {
        case i: IRI => schema.qualify(i)
        case l: Literal => l.getLexicalForm
        case b: BNodeId => "_:" + b.getLexicalForm
      }
    }
  }

  lazy val emptyTyping: ShapeTyping =
    Monoid[ShapeTyping].empty

}

object Validator {

  def empty = Validator(schema = Schema.empty)

  type Result[A] = Xor[NonEmptyList[ViolationError], List[(A, Evidences)]]

  def isOK[A](r: Result[A]): Boolean =
    r.isRight && r.toList.isEmpty == false

}

