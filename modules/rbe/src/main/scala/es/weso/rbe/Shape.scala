package es.weso.rbe
import es.weso.utils.Read
import es.weso.rbe.nodeShape._

abstract class Shape[Edge,Node,Label,Err,Evidence]

/**
  * A shape contains a regular bag expression, a closed modifier and a list of extras
  *
  * @param rbe regular bag expression
  * @param extras list of extra edges that are allowed
  * @param closed the shape is closed
  */
case class SingleShape[Edge,Node,Label,Err,Evidence](
    nodeShape: NodeShape[Node,Label,Err,Evidence],
    rbe: Rbe[(Edge,NodeShape[Node,Label,Err,Evidence])], 
    extras: Seq[Edge], 
    closed: Boolean
) extends Shape[Edge,Node,Label,Err,Evidence]

case class AndShape[Edge,Node,Label,Err,Evidence](
   s1: Shape[Edge,Node,Label,Err,Evidence],
   s2: Shape[Edge,Node,Label,Err,Evidence]) extends Shape[Edge,Node,Label,Err,Evidence]

/*case class OrShape[Edge,Node,Label,Err](
   s1: Shape[Edge,Node,Label,Err],
   s2: Shape[Edge,Node,Label,Err]) extends Shape[Edge,Node,Label,Err] */

case class NotShape[Edge,Node,Label,Err,Evidence](
   s: Shape[Edge,Node,Label,Err,Evidence]) extends Shape[Edge,Node,Label,Err,Evidence]

object Shape {

  def empty[Edge,Node,Label,Err, Evidence: Read]: SingleShape[Edge,Node,Label,Err,Evidence] = 
    SingleShape(
     nodeShape = NodeShape.any,
     rbe = Empty,
     extras = Seq(),
     closed = false
    )

  def singleShape[Edge,Node,Label,Err,Evidence: Read](
      rbe: Rbe[(Edge,NodeShape[Node,Label,Err,Evidence])],
      extras: Seq[Edge] = Seq(),
      closed: Boolean = false): SingleShape[Edge,Node,Label,Err,Evidence] =
    SingleShape(
     nodeShape = NodeShape.any,
     rbe = rbe,
     extras = extras,
     closed = closed)

}
