package es.weso.shex.spec

import cats._
import cats.implicits._
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, Literal, RDFNode}
import es.weso.shapeMaps.{BNodeLabel => BNodeShapeMapLabel, IRILabel => IriShapeMapLabel, _}
import es.weso.shex._
import Check._
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex.validator.Arc
import es.weso.typing.Typing
import es.weso.utils.SetUtils

object Spec extends LazyLogging {
  def logInfo(str: String):Unit = println(str)

 /* def fixedShapeMap2Typing(m: FixedShapeMap): ShapeTyping = {
    val zero: ShapeTyping = TypingMap.empty
    def cmb(next: ShapeTyping, current:(RDFNode,ShapeMapLabel,Status)): ShapeTyping = {
      val (node,lbl,status) = current
      status match {
        case Conformant => next.addType(node,lbl)
//        case NonConformant => next.add
      }
    }
    m.flatten.foldLeft(zero)(cmb)
  }
*/
/*  def checkShapeMap(rdf: RDFReader, m: FixedShapeMap): Check[Boolean] = {
    val typing = fixedShapeMap2Typing(m)
    def checkAll: Check[Boolean] = {
      val ls: List[Check[Boolean]] = m.flatten.map{ case (node,lbl,status) => status match {
        case Conformant => satisfiesLabel(node,lbl)
        case NonConformant => notSatisfiesLabel(node,lbl)
      } }
      satisfyAll(ls)
    }
    runLocalWithTyping(_ => typing, checkAll)
  }
*/

  def satisfyStatus(node: RDFNode, lbl: ShapeMapLabel, status: Status): Check[Boolean] = ???

  /*{
    m.flatten.map(t => t._3 match {
      case Conformant => satisfiesLabel(t._1,t._2,rdf)
    })
  } */

  def satisfiesLabel(node: RDFNode, lbl: ShapeMapLabel): Check[Boolean] = for {
     se <- getShapeFromShapeMapLabel(lbl)
     r <- runLocalWithTyping(_.addConformant(node,lbl), satisfies(node,se))
  } yield r

  def notSatisfiesLabel(node: RDFNode, lbl: ShapeMapLabel): Check[Boolean] = for {
    se <- getShapeFromShapeMapLabel(lbl)
    r <- runLocalWithTyping(_.addConformant(node,lbl), satisfies(node,se))
  } yield !r


  def satisfies(n: RDFNode, se: ShapeExpr): Check[Boolean] = {
    logInfo(s"satisfies(${n.show},${se.show})?")
    se match {
      case nc: NodeConstraint => satisfies2(n, nc)
      case ShapeAnd(_,ses) => satisfyAll(ses.map(satisfies(n,_)))
      case ShapeOr(_,ses) => satisfySome(ses.map(satisfies(n,_)))
      case ShapeNot(_,se) => satisfyNot(satisfies(n,se))
      case ShapeRef(lbl) => satisfyShapeRef(n,lbl)
      case ShapeExternal(id) => unimplemented(s"ShapeExpternal($id)")
      case s: Shape => satisfyShape(n,s)
    }
  }

  def satisfyShapeRef(n: RDFNode, lbl: ShapeLabel): Check[Boolean] =
   for {
    se <- getShape(lbl)
    v <- satisfies(n,se)
   } yield v

  def satisfyShape(n: RDFNode, shape: Shape): Check[Boolean] = {
    logInfo(s"satisfyShape(${n.show},${shape.show})")
    shape.expression match {
      case None => pure(true)
      case Some(te) => {
        for {
          neighs <- neighs(n)
          b <- satisfyFirst(SetUtils.pSet(neighs), satisfyMatches(te))
        } yield {
          logInfo(s"> satisfyShape(${n.show},${shape.show})=$b")
          b
        }
      }
    }
  }

  private def showArcs(arcs: Set[Arc]) = arcs.map(_.show).mkString(",")

  def satisfyMatches(te: TripleExpr)(pair: (Set[Arc], Set[Arc])): Check[Boolean] = {
    val (matched,remainder) = pair
    logInfo(s"satisfyMatches(te=$te,matched=${showArcs(matched)},remainder=${showArcs(remainder)}")
    val r = matches(matched,te)
    logInfo(s"> satisfyMatches(${te.show}, ${showArcs(matched)}, ${showArcs(remainder)}) = $r")
    r
    // TODO: rest of conditions
  }

  def matches(matched: Set[Arc],
              te: TripleExpr
             ): Check[Boolean] = {
    logInfo(s"matches(matched=${showArcs(matched)},$te)")
    te match {
      case oneOf: OneOf => matchesOneOf(matched, oneOf)
      case eachOf: EachOf => matchesEachOf(matched, eachOf)
      case tc: TripleConstraint => matchesTripleConstraint(matched, tc)
      case i: Inclusion => matchesInclusion(matched, i)
      case e: Expr => matchesExpr(matched, e)
    }
  }

  def matchesOneOf(matched: Set[Arc], oneOf: OneOf): Check[Boolean] =
    unimplemented(s"matchedOneOf $oneOf")

  def matchesEachOf(matched: Set[Arc], eachOf: EachOf): Check[Boolean] =
    unimplemented(s"matchedEachOf $eachOf")

  def matchesTripleConstraint(matched: Set[Arc],
                              tc: TripleConstraint
                             ): Check[Boolean] =
   {
     logInfo(s"matchesTripleConstraint($matched,$tc)")
     matched.size match {
       case 0 =>
         if (tc.min == 0) pure(true)
         else pure(false)
       case 1 => matchTripleConstraint(matched.head, tc)
       case n =>
         satisfyOr(
           satisfyAnd(
             matchTripleConstraint(matched.head,tc),
             matchesTripleConstraint(matched.tail,tc.decreaseCard)
           ),
           matchesTripleConstraint(matched.tail, tc)
         )
     }
   }

  def matchTripleConstraint(arc: Arc,
                            tc: TripleConstraint
                           ): Check[Boolean] = {
    logInfo(s"matchTripleConstraint(${arc.show},${tc.show})")
    if (tc.direct) {
      arc match {
        case Arc(Direct(p), n) if (p == tc.predicate) =>
          tc.valueExpr match {
            case None => pure(true)
            case Some(se) => satisfies(n, se)
          }
        case _ => pure(false)
      }
    }
    else {
      arc match {
        case Arc(Inverse(p), n) if p == tc.predicate =>
          tc.valueExpr match {
            case None => pure(true)
            case Some(se) => satisfies(n, se)
          }
      }
    }
  }

  def matchesExpr(matched: Set[Arc], e: Expr): Check[Boolean] =
    unimplemented(s"matchesExpr $e")

  def matchesInclusion(matched: Set[Arc], i: Inclusion): Check[Boolean] =
    unimplemented(s"matchesInclusion $i")

  def neighs(n: RDFNode): Check[Set[Arc]] = for {
    rdf <- getRDF
  } yield {
    val outArcs = rdf.triplesWithSubject(n).map(t => Arc(Direct(t.pred),t.obj))
    val inArcs = rdf.triplesWithObject(n).map(t => Arc(Inverse(t.pred),t.obj))
    val allArcs = outArcs ++ inArcs
    logInfo(s"neighs($n): ${allArcs.map(_.show).mkString(",")}")
    allArcs
  }

  def satisfies2(n: RDFNode, nc: NodeConstraint): Check[Boolean] = nodeSatisfies(n,nc)

  def nodeSatisfies(n: RDFNode, nc: NodeConstraint): Check[Boolean] = for {
    nk <- optSatisfy(nc.nodeKind, satisfiesNodeKind(n))
    dt <- optSatisfy(nc.datatype, satisfiesDatatype(n))
  } yield List(nk,dt).forall(_ == true)

  def satisfiesNodeKind(n: RDFNode)(nk: NodeKind): Check[Boolean] = nk match {
    case IRIKind => pure(n.isIRI)
    case BNodeKind => pure(n.isBNode)
    case LiteralKind => pure(n.isLiteral)
    case NonLiteralKind => pure(n.isNonLiteral)
  }

  def satisfiesDatatype(n: RDFNode)(dt: IRI): Check[Boolean] = n match {
    case l: Literal => pure(l.dataType == dt)
    case _ => pure(false)
  }

  def getShape(lbl: ShapeLabel): Check[ShapeExpr] = for {
    schema <- getSchema
    shape <- schema.getShape(lbl) match {
      case None => err(s"Not found $lbl in schema")
      case Some(se) => pure(se)
    }
  } yield shape

  def getShapeFromShapeMapLabel(lbl: ShapeMapLabel): Check[ShapeExpr] = for {
    schema <- getSchema
    shape <- lbl match {
      case Start => schema.start match {
        case None => err(s"Not found Start in schema")
        case Some(se) => pure(se)
      }
      case IriShapeMapLabel(iri) => getShape(IRILabel(iri))
      case BNodeShapeMapLabel(bn) => getShape(BNodeLabel(bn))
    }
  } yield shape


}