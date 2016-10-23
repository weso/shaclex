package es.weso.rbe
import es.weso.typing._
import cats._, data._
import cats.implicits._

case class SingleResult[Edge,Node,Label,Err,Evidence](
    typing: Typing[Node,Label,Err,Evidence],
    rest: Set[(Node,Edge,Node)]) {


  implicit val srMonoid = new Monoid[SingleResult[Edge,Node,Label,Err,Evidence]] {
    override def empty: SingleResult[Edge,Node,Label,Err,Evidence] = 
      SingleResult(Typing.empty, Set())
      
    override def combine(
        r1: SingleResult[Edge,Node,Label,Err,Evidence],
        r2: SingleResult[Edge,Node,Label,Err,Evidence]): SingleResult[Edge,Node,Label,Err,Evidence] = {
      SingleResult(r1.typing.combineTyping(r2.typing), r1.rest ++ r2.rest)
    }
          
  }

} 
    

