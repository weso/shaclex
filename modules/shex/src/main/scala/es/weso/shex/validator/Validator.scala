package es.weso.shex.validator
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._
// import util.matching._
import es.weso.shex.implicits.showShEx._
import ViolationError._
import es.weso.rdf.PREFIXES._
import es.weso.shex.validator.table._

/**
 * ShEx validator
 */
case class Validator(schema: Schema) extends LazyLogging {

  import es.weso.checking._
  import Validator._

  lazy val sh_targetNode = sh + "targetNode"

  object MyChecker extends CheckerCats {

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
  }

  type ShapeChecker = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping

  import MyChecker._

  type CheckTyping = Check[ShapeTyping]
  type NodeChecker = Attempt => RDFNode => CheckTyping

  def checkTargetNodeDeclarations: CheckTyping = for {
    rdf <- getRDF
    nodeLabels <- getTargetNodeDeclarations(rdf)
    ts <- checkAll(
      nodeLabels.map{
       case (node,label) => checkNodeLabel(node,label)
      })
    t <- combineTypings(ts)
  } yield t

  // TODO
  def getTargetNodeDeclarations(
    rdf: RDFReader
  ): Check[List[(RDFNode,ShapeLabel)]] = {
    checkAll(rdf.triplesWithPredicate(sh_targetNode).
      map(t => (t.obj,mkShapeLabel(t.subj))).
      toList.map(checkPair2nd)
    )
  }

  def mkShapeLabel(n: RDFNode): Check[ShapeLabel] = {
    n match {
      case i:IRI => ok(IRILabel(i))
      case b:BNodeId => ok(BNodeLabel(b))
      case _ => {
        errStr(s"mkShapeLabel: Node $n can't be a shape")
      }
    }
  }

  def getShape(label: ShapeLabel): Check[ShapeExpr] =
    schema.getShape(label) match {
      case None => errStr[ShapeExpr](s"Can't find shape $label is Schema:\n${schema.show}")
      case Some(shape) => ok(shape)
    }

  def checkNodeLabel(node: RDFNode, label: ShapeLabel): CheckTyping = {
    logger.info(s"nodeLabel. Node: $node Label: $label")
    for {
      typing <- getTyping
      newTyping <- if (typing.hasType(node, label))
        ok(typing)
      else if (typing.hasNoType(node, label)) {
        errStr[ShapeTyping](s"Failed because $node doesn't match shape $label")
      } else for {
        shapeExpr <- getShape(label)
        shapeType = ShapeType(shapeExpr, Some(label))
        attempt = Attempt(NodeShape(node, shapeType), None)
        t <- runLocal(checkNodeShapeExpr(attempt, node, shapeExpr), _.addType(node, shapeType))
      } yield t
    } yield newTyping
  }

  def errStr[A](msg: String): Check[A] =
    err[A](ViolationError.msgErr(msg))

  def checkNodeShapeExpr(attempt: Attempt, node: RDFNode, s: ShapeExpr): CheckTyping = {
    logger.info(s"Attempt: $attempt, node: $node shapeExpr: $s")
    s match{
      case ShapeOr(ses) => checkOr(attempt,node,ses)
      case ShapeAnd(ses) => checkAnd(attempt,node,ses)
      case ShapeNot(s) => checkNot(attempt,node,s)
      case nc: NodeConstraint => checkNodeConstraint(attempt,node,nc)
      case s: Shape => checkShape(attempt,node,s)
      case ShapeRef(ref) => checkRef(attempt,node,ref)
      case s: ShapeExternal => errStr(s"Not implemented ShapeExternal $attempt")
    }
  }

  def checkAnd(attempt: Attempt,
               node: RDFNode,
               ses:List[ShapeExpr]): CheckTyping = for {
    ts <- checkAll(ses.map(se => checkNodeShapeExpr(attempt,node,se)))
    t <- combineTypings(ts)
  } yield t

  def checkOr(attempt: Attempt,
               node: RDFNode,
               ses:List[ShapeExpr]): CheckTyping = {
    val vs = ses.map(se => checkNodeShapeExpr(attempt,node,se))
    for {
      t1 <- checkSome(vs,ViolationError.msgErr(s"None of the alternatives of OR($ses) is valid for node $node"))
      t2 <- addEvidence(attempt.nodeShape, s"$node passes OR")
      t3 <- combineTypings(Seq(t1, t2))
    } yield t3
  }

  def checkNot(attempt: Attempt,
               node: RDFNode,
               s: ShapeExpr): CheckTyping = {
    val parentShape = attempt.nodeShape.shape
    val check: CheckTyping = checkNodeShapeExpr(attempt, node, s)
    val handleError: ViolationError => Check[ShapeTyping] = e => for {
      t1 <- addNotEvidence(NodeShape(node, ShapeType(s, None)), e,
        s"$node doesn't satisfy ${s}. Negation declared in ${parentShape}. Error: $e")
      t2 <- addEvidence(attempt.nodeShape, s"$node satisfies not(${s})")
      t <- combineTypings(List(t1, t2))
    } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      errStr(s"Failed NOT($s) because node $node satisfies it")
    cond(check, handleNotError, handleError)
  }

  def checkRef(attempt: Attempt,
               node: RDFNode,
               ref:ShapeLabel): CheckTyping =
    checkNodeLabel(node,ref)

  def checkNodeConstraint(attempt: Attempt,
                          node: RDFNode,
                          s: NodeConstraint): CheckTyping =
  for {
    _ <- getTyping
    t1 <- optCheck(s.nodeKind, checkNodeKind(attempt,node), getTyping)
  } yield t1


  def checkNodeKind(attempt: Attempt, node: RDFNode)(nk: NodeKind): CheckTyping = {
    logger.info(s"NodeKind $node $nk")
    nk match {
      case IRIKind => {
        logger.info(s"checking if $node is an IRI")
        checkCond(node.isIRI, attempt,
          msgErr(s"$node is not an IRI"), s"$node is an IRI")
      }
      case BNodeKind =>
        checkCond(node.isBNode, attempt,
          msgErr(s"$node is not a BlankNode"), s"$node is a BlankNode")
      case NonLiteralKind =>
        checkCond(! node.isLiteral, attempt,
          msgErr(s"$node is a literal but should be a NonLiteral"),
          s"$node is NonLiteral")
      case LiteralKind =>
        checkCond(node.isLiteral, attempt,
          msgErr(s"$node is not an Literal"),
          s"$node is a Literal")
    }
  }

  def checkShape(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping =
   // TODO: virtual, extra, etc....
   for {
   t <- optCheck(s.expression,checkTripleExpr(attempt,node),getTyping)
  } yield t

  def checkTripleExpr(
    attempt: Attempt,
    node: RDFNode)
    (t: TripleExpr): CheckTyping = for {
    rdf <- getRDF
    cTable = CTable.mkTable(t)
    neighs = getNeighs(node,rdf)
  } yield ???

  def getNeighs(node: RDFNode, rdf: RDFReader): List[(Path,RDFNode)] = {
    ???
  }

/*    t match {
    case e: EachOf => errStr(s"Not implemented eachOf $attempt $node")
    case s: SomeOf => errStr(s"Not implemented eachOf $attempt $node")
    case i: Inclusion => errStr(s"Not implemented inclusion $attempt $node")
    case tc: TripleConstraint =>
      checkTripleConstraint(attempt,node)(tc)
  } */

def checkTripleConstraint(
    attempt: Attempt,
    node: RDFNode)
    (t: TripleConstraint): CheckTyping = for {
  rdf <- getRDF
  triples =
    if (t.direct) rdf.triplesWithSubjectPredicate(node,t.predicate)
    else rdf.triplesWithPredicateObject(t.predicate,node)
  noTriples = triples.size
  t <- checkCond(noTriples >= t.min && t.max.biggerThanOrEqual(noTriples),
           attempt,
           msgErr(s"Number of triples with predicate ${t.predicate}=$noTriples not in (${t.min},${t.max})"),
           s"No. of triples with predicate ${t.predicate}= $noTriples between (${t.min},${t.max})")
 } yield t

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

  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult[ViolationError, ShapeTyping, Log] = {
    implicit def showPair = new Show[(ShapeTyping, Evidences)] {
      def show(e: (ShapeTyping, Evidences)): String = {
        s"Typing: ${e._1}\n Evidences: ${e._2}"
      }
    }
    runCheck(checkTargetNodeDeclarations, rdf)
  }

}

object Validator {

  def empty = Validator(schema = Schema.empty)

  type Result[A] = Xor[NonEmptyList[ViolationError], List[(A, Evidences)]]

  def isOK[A](r: Result[A]): Boolean =
    r.isRight && r.toList.isEmpty == false

}

