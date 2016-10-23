package es.weso.rbe

import es.weso.typing._
import es.weso.checking._
import es.weso.utils.SeqUtils._
import es.weso.rbe.interval._
import es.weso.collection.Bag
import cats._, data._
import implicits._

trait IterativeMatcher[Edge, Node, Label] extends Matcher[Edge, Node, Label, String] {

  type Evidence = String
  type Err = RbeError
  type Shape_ = Shape[DirectedEdge[Edge], Node, Label, RbeError, Evidence]
  type NodeShape_ = NodeShape[Node, Label, Err, Evidence]
  type SingleShape_ = SingleShape[DirectedEdge[Edge], Node, Label, RbeError, Evidence]
  type Candidate_ = Candidate[Edge, Node, Label, RbeError, Evidence]
  type Candidates_ = Seq[Candidate_]
  type Neigh_ = Neigh[Edge, Node]
  type Neighs_ = Seq[Neigh_]
  type Table_ = Table[Edge, Node, Label, RbeError, Evidence]
  type Triple_ = (Node, Edge, Node)

  def matchNodeLabel(
    node: Node,
    label: Label): CheckTyping = {
    val attempt = Attempt(node, label)
    for {
      _ <- logStr(attempt, s"matchNodeLabel: $node with $label")
      schema <- getSchema
      t <- getTyping
      nt <- if (t.hasType(node, label)) ok(t)
      else schema.m.get(label) match {
        case None        => checkErrStr(s"$label doesn't have shape associated in schema $schema")
        case Some(shape) => matchNodeShape(node, shape, attempt)
      }
    } yield nt
  }

  def matchNodeShape(node: Node, shape: Shape_, attempt: Attempt_): CheckTyping = shape match {
    case s: SingleShape_ =>
      logStr(attempt, s"Matching $node with $shape") >>
        matchNodeSingleShape(node, s, attempt)
    case _ =>
      checkErrStr(s"Unsupported matching of $node with complex shape $shape. Current attempt: $attempt")
  }

  def matchNodeSingleShape(node: Node, shape: SingleShape_, attempt: Attempt_): CheckTyping = {

    def matchCandidates(cs: Candidates_): CheckTyping = for {
      _ <- logStr(attempt, s"Trying to match $node with candidates $cs")
      t <- local(_.addType(node, attempt.label,
        List(s"$node asserted of type ${attempt.label}")))(matchNodeCandidates(node, attempt, cs))
    } yield t

    for {
      graph <- getGraph
      val neighs = graph.neighbours(node)
      _ <- logStr(attempt, s"Neighs of $node = $neighs")
      val (table, sorbe) = Table.mkTable(shape)
      _ <- logStr(attempt, s"Table: $table")
      _ <- logStr(attempt, s"Sorbe: $sorbe")
      val open = !shape.closed
      candidates <- calculateCandidates(table, neighs, sorbe, node, open, shape.extras, attempt)
      _ <- logStr(attempt, s"Candidates: $candidates")
      t <- checkFirst(candidates.toList, matchCandidates _, errStr("No candidate matches"))
    } yield t
  }

  def matchNodeCandidates(node: Node, attempt: Attempt_, candidates: Candidates_): CheckTyping = for {
    t <- getTyping
    _ <- logStr(attempt, s"matching $node with candidates $candidates...?")
    result <- resolveCandidates(candidates, node, attempt)
  } yield result

  /**
   * Resolve candidate
   *
   * @param n node to resolve
   * @param g graph
   * @param c Candidate
   * @param rest current result
   */
  private def resolveCandidate(n: Node, attempt: Attempt_)(c: Candidate_): CheckTyping = c match {
    case Missing(_, _, _) => for {
      t <- getTyping
      _ <- logStr(attempt, "Missing candidate $c")
    } yield t
    case Pending(c, obj, label, arc, _) => for {
      _ <- logStr(attempt, s"Pending candidate $c. Node $obj - Label: $label") 
      _ <- addArc(arc)
      t <- matchNodeLabel(obj, label)
    } yield t
    case Pos(ref, arc, _) => for {
      _ <- logStr(attempt, s"Positive match $ref with arc $arc") 
      _ <- addArc(arc) 
      t <- getTyping
    } yield t
    case _                => checkErrStr(s"Unimplemented resolveCandidate $c on node $n")
  }

  def resolveCandidates(cs: Candidates_, node: Node, attempt: Attempt_): CheckTyping = for {
    _ <- logStr(attempt,s"Resolving candidates $cs for $node")
    ts <- checkList(cs.toList, resolveCandidate(node, attempt))
    val t = Typing.combineTypings(ts.toSeq) 
    _ <- logStr(attempt, s"Resulting typing $t")
  } yield t

  private def addArc(arc: Triple_): Check[Unit] = for {
    _ <- updateInfo(_ + arc)
  } yield (())

    
  /*
      case PendingNot(c, obj, label, arc, _) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => noMatchNodeInTyping(obj, label, result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult2(arc, r)
        }
      }

      case PendingSeq(c, obj, labels, arc, _) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInAllLabelsTyping(obj, labels, result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult2(arc, r)
        }
      }

      case PendingAlt(c, obj, labels, arc, _) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInSomeLabelsTyping(obj, labels, result))
          ConsoleDebugger.debugStep(s"rs: $rs")
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult2(arc, r)
        }
      }

      case PendingOr(c, obj, vs, arc, _) => rest match {
        case Failure(e) => Failure(e)
        case Success(results) => {
          val rs = results.map(result => matchNodeInSomeTyping(obj, vs, result))
          val f = filterSuccess(rs)
          val r = f.map(t => t.flatten)
          addArcResult2(arc, r)
        }
      }

      case Neg(ref, arc, _, es) => {
        // TODO: Throw exception?
        rest
      }

    }
} */

  def possibleCandidates(table: Table_, node: Node, neigh: Neigh_, attempt: Attempt_): Check[Candidates_] = {
    val edge = neigh.directedEdge
    val nodeToCheck = neigh.node
    val constraints = lookupEdgeConstraints(table, edge)
    val checks = constraints.map(c => checkCandidate(lookupConstraintShape(table, c), node, edge, nodeToCheck, c, attempt)).toList
    for {
      r <- checks.sequence
    } yield r
  }

  /**
   * Calculates the candidates of a node
   *
   * @param table Shape table
   * @param node node to calculate candidates
   * @param neighs neighbours of a node
   */
  def candidates(table: Table_,
                 node: Node,
                 neighs: Neighs_,
                 attempt: Attempt_): Check[List[Candidates_]] = for {
    _ <- logStr(attempt, s"Calculating candidates of $node, table: $table, neighs: $neighs")
    val ps: List[Check[Candidates_]] = neighs.map(possibleCandidates(table, node, _, attempt)).toList
    r <- ps.sequence
  } yield r

  private def calculateCandidates(
    table: Table_,
    out: Neighs_,
    rbe: Rbe[ConstraintRef],
    node: Node,
    open: Boolean,
    extras: Seq[DirectedEdge[Edge]],
    attempt: Attempt_): Check[Seq[Candidates_]] = for {
    cs <- filterCandidates(table, out, node, rbe, open, extras, attempt)
  } yield cs

  private def checkCandidate(
    shape: NodeShape_,
    node: Node,
    edge: DirectedEdge[Edge],
    nodeToCheck: Node,
    c: ConstraintRef,
    attempt: Attempt_): Check[Candidate_] = {
    shape match {
      case Ref(label) =>
        ok(Pending(c, nodeToCheck, label, mkArc(edge, node, nodeToCheck), edge))

      case RefNot(label) =>
        ok(PendingNot(c, nodeToCheck, label, mkArc(edge, node, nodeToCheck), edge))

      case DisjRef(labels) =>
        ok(PendingAlt(c, nodeToCheck, labels, mkArc(edge, node, nodeToCheck), edge))

      case OrShape(vs) =>
        ok(PendingOr(c, nodeToCheck, vs, mkArc(edge, node, nodeToCheck), edge))

      case ConjRef(labels) =>
        ok(PendingSeq(c, nodeToCheck, labels, mkArc(edge, node, nodeToCheck), edge))

      case p: Pred[Node, Err, Evidence] =>
        matchPred(p, nodeToCheck, node, edge, c, attempt)

      case _ => throw new Exception(s"testCandidate: unknown shape $shape")
    }
  }

  def matchPred(p: Pred[Node, Err, Evidence],
                nodeToCheck: Node,
                node: Node,
                edge: DirectedEdge[Edge],
                c: ConstraintRef,
                attempt: Attempt_): Check[Candidate_] = {
    val checked = p.pred(nodeToCheck)
    val arc = mkArc(edge, node, nodeToCheck)
    for {
      _ <- logStr(attempt, s"Checking pred $p on node $nodeToCheck...node: $node")
      r <- checked.fold(mkPos(arc, edge, c), mkNeg(arc, edge, c))
    } yield r
  }

  def mkPos(arc: Triple_, edge: DirectedEdge[Edge], c: ConstraintRef)(p: (Node, Evidence)): Check[Candidate_] =
    ok(Pos(c, arc, edge))

  def mkNeg(arc: Triple_, edge: DirectedEdge[Edge], c: ConstraintRef)(es: NonEmptyList[RbeError]): Check[Candidate_] =
    ok(Neg(c, arc, edge, es.toList.toSeq))

  private def mkArc(
    directedEdge: DirectedEdge[Edge],
    n1: Node, n2: Node): Triple_ = {
    directedEdge match {
      case DirectEdge(edge)  => (n1, edge, n2)
      case InverseEdge(edge) => (n2, edge, n1)
    }
  }

  private def lookupConstraintShape(
    table: Table_,
    c: ConstraintRef): NodeShape_ = {
    table.constraints.get(c) match {
      case None =>
        throw new Exception(s"Cannot find constraintRef $c in table $table")
      case Some(nodeShape) =>
        nodeShape
    }
  }

  private def lookupEdgeConstraints(
    table: Table_,
    directedEdge: DirectedEdge[Edge]): Seq[ConstraintRef] = {
    table.edges.get(directedEdge).getOrElse(Set()).toSeq
  }

  def filterCandidates(
    table: Table_,
    out: Neighs_,
    node: Node,
    Rbe: Rbe[ConstraintRef],
    open: Boolean,
    extras: Seq[DirectedEdge[Edge]],
    attempt: Attempt_): Check[Seq[Candidates_]] = for {
    css <- zipCandidates(table, node, out, attempt)
  } yield css.filter(cs => matchCandidateRbe(cs, Rbe, open, extras))

  def zipCandidates(table: Table_, node: Node, out: Neighs_, attempt: Attempt_): Check[Seq[Seq[Candidate_]]] = for {
    cs <- candidates(table, node, out, attempt)
  } yield zipN(cs)

  private def matchCandidateRbe(cs: Seq[Candidate_],
                                rbe: Rbe[ConstraintRef],
                                open: Boolean,
                                extras: Seq[DirectedEdge[Edge]]): Boolean = {
    val bag = candidatesToBag(cs)
    val intervalChecker = IntervalChecker(rbe)
    if (containsContradictions(cs, extras)) false
    else {
      val checked = intervalChecker.check(bag, open)
      checked.isRight
    }

  }

  // TODO: The following code could be optimized using some mathematical formula
  // A contradiction appears when a value N has sign -1 and another value N has sign +1
  // allow contradictions if the predicate belongs to EXTRAs
  private def containsContradictions(
    cs: Seq[Candidate_],
    extras: Seq[DirectedEdge[Edge]]): Boolean = {
    val noExtras = cs.filter(c => !(extras contains c.edge))
    // val pos = noExtras.filter(_.sign == 1).map(_.value)
    val neg = noExtras.filter(_.sign == -1).map(_.value)
    //    pos.intersect(neg).length != 0
    neg.length != 0
  }

  // TODO: It ignores extra predicates (value None) and negative candidates by now
  private def candidatesToBag(cs: Seq[Candidate_]): Bag[ConstraintRef] = {
    Bag.toBag(cs.filter(_.sign == 1).map(_.value))
  }

  def errStr(msg: String): RbeError =
    RbeError(msg)

  def checkErrStr(msg: String): CheckTyping =
    err(errStr(msg))

  def checkFirst[A, B](cs: List[A], checker: A => Check[B], errorIfNone: Err): Check[B] =
    checkSome(cs.map(checker), errorIfNone)

  def logStr(attempt: Attempt_, msg: String): Check[Unit] =
    addLog(MatcherLog(List((attempt, msg))))

}