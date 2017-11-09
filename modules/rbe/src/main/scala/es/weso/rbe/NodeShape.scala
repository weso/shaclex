package es.weso.rbe

import cats._
import implicits._
import es.weso.utils.Read

object nodeShape {

  type CheckVal[Node, Err, Evidence] = Either[List[Err], (Node, Evidence)]

  /**
   * A node shape
   */
  sealed trait NodeShape[+Node, +Label, Err, Evidence] {
  }

  /**
   *  Reference to another label
   */
  case class Ref[Label, Err, Evidence](label: Label) extends NodeShape[Nothing, Label, Err, Evidence] {
  }

  /**
   * Negation of a expression
   */
  case class RefNot[Label, Err, Evidence](label: Label) extends NodeShape[Nothing, Label, Err, Evidence]

  /**
   *  Reference to a sequence of labels
   */
  case class ConjRef[Label, Err, Evidence](labels: Seq[Label]) extends NodeShape[Nothing, Label, Err, Evidence]

  /**
   *  Reference to an sequence of labels which are disjunctive
   */
  case class DisjRef[Label, Err, Evidence](labels: Seq[Label]) extends NodeShape[Nothing, Label, Err, Evidence]

  case class OrShape[+Node, +Label, Err, Evidence](
    ns: Seq[NodeShape[Node, Label, Err, Evidence]]) extends NodeShape[Node, Label, Err, Evidence]

  /**
   * Constraint on nodes (it has a name and a predicate).
   *
   * Note: pred is defined in the 2nd parameter section to avoid equality and
   * hashing of functions
   *
   */
  // TODO: Previous type of pred: Node => CheckedVal[Node,List[Node]]
  case class Pred[Node, Err, Evidence](name: String)(val pred: Node => CheckVal[Node, Err, Evidence]) extends NodeShape[Node, Nothing, Err, Evidence]

  /**
   * Some common node shapes
   */
  object NodeShape {

    type CheckVal[Node, Err, Evidence] = Either[List[Err], (Node, Evidence)]

    /**
     * any = any value matches, so no constraint at all
     */
    def any[Node, Label, Err, Evidence: Read]: Pred[Node, Err, Evidence] =
      Pred("any")((node) => ok(node, s"$node satisfies constraint any"))

    def ok[A, Err, Evidence: Read](x: A, msg: String): CheckVal[A, Err, Evidence] =
      Either.right((x, implicitly[Read[Evidence]].read(msg)))

    def errString[A, Evidence: Read](err: String): CheckVal[A, RbeError, Evidence] =
      Either.left(List(RbeError(err)))

  }

}