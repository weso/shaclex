package es.weso.shex.spec

import cats._
import cats.implicits._
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes.{IRI, Literal, RDFNode}
import es.weso.shapeMaps.{
  BNodeLabel => BNodeShapeMapLabel,
  Conformant => ConformantStatus,
  IRILabel => IriShapeMapLabel,
  Info => ShapeMapInfo,
  NonConformant => NonConformantStatus,
  Start => StartMapLabel,
  _
}
import es.weso.shex._
import Check._
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex.validator.Arc
import es.weso.utils.{LogInfo, SetUtils}

object Spec extends LazyLogging {
  def logInfo(str: String, incr: Int):Unit = LogInfo(str, incr)

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
  def checkNodeLabelStatus: ((RDFNode, ShapeMapLabel, Status)) => Check[ShapeTyping] = t => {
    val (node, lbl, status) = t
    status match {
      case ConformantStatus =>
        runLocalWithTyping(_.addTesting(node,lbl), satisfiesLabel(node,lbl))
      case NonConformantStatus =>
        runLocalWithTyping(_.addTesting(node,lbl), notSatisfiesLabel(node,lbl))
    }
  }

  def checkShapeMap(rdf: RDFReader, m: FixedShapeMap): Check[ShapeTyping] = {
    val ls : List[(RDFNode, ShapeMapLabel, Status)] = m.flatten
    satisfyChain(ls,checkNodeLabelStatus)
  }


  def satisfyStatus(node: RDFNode, lbl: ShapeMapLabel, status: Status): Check[Boolean] = ???

  /*{
    m.flatten.map(t => t._3 match {
      case Conformant => satisfiesLabel(t._1,t._2,rdf)
    })
  } */

  def satisfiesLabel(node: RDFNode, lbl: ShapeMapLabel): Check[ShapeTyping] = for {
     typing <- getTyping
     se <- getShapeFromShapeMapLabel(lbl)
     b <- runLocalWithTyping(_.addConformant(node,lbl, List()), satisfies(node,se))
     newTyping <- if (b) {
       fromEither(typing.addConformant(node,lbl,List(s"Node $node satisfies $lbl")))
     } else {
       fromEither(typing.addNonConformant(node,lbl,List(s"Node $node does not satisfy $lbl")))
     }
  } yield newTyping


  def notSatisfiesLabel(node: RDFNode, lbl: ShapeMapLabel): Check[ShapeTyping] = for {
    typing <- getTyping
    se <- getShapeFromShapeMapLabel(lbl)
    b <- runLocalWithTyping(_.addNonConformant(node,lbl,List()), notSatisfies(node,se))
    newTyping <- if (b) {
      fromEither(typing.addNonConformant(node,lbl,List(s"Node $node does not satisfy $lbl")))
    } else {
      fromEither(typing.addConformant(node,lbl,List(s"Node $node does satisfy $lbl")))
    }
  } yield newTyping



  def satisfies(n: RDFNode, se: ShapeExpr): Check[Boolean] = {
    logInfo(s"satisfies(${n.show},${se.show})?",1)
    val r = se match {
      case nc: NodeConstraint => satisfies2(n, nc)
      case ShapeAnd(_,ses,_,_) => satisfyAll(ses.map(satisfies(n,_)))
      case ShapeOr(_,ses,_,_) => satisfySome(ses.map(satisfies(n,_)))
      case ShapeNot(_,se,_,_) => satisfyNot(satisfies(n,se))
      case ShapeRef(lbl,_,_) => satisfyShapeRef(n,lbl)
      case ShapeExternal(id,_,_) => unimplemented(s"ShapeExpternal($id)")
      case s: Shape => satisfyShape(n,s)
    }
    for {
      b <- r
    } yield {
      logInfo(s"Result satisfies(${n.show},${se.show})=$b",-1)
      b
    }
  }

  def notSatisfies(n: RDFNode, se: ShapeExpr): Check[Boolean] = for {
    b <- satisfies(n,se)
  } yield (!b)

  def satisfyShapeRef(n: RDFNode, lbl: ShapeLabel): Check[Boolean] =
   for {
    se <- getShape(lbl)
    v <- satisfies(n,se)
   } yield v

  def satisfyShape(n: RDFNode, shape: Shape): Check[Boolean] = {
    logInfo(s"satisfyShape(${n.show},${shape.show})",1)
    shape.expression match {
      case None => {
        logInfo(s"result of satisfyShape(${n.show},None)=true",-1)
        pure(true)
      }
      case Some(te) => {
        for {
          neighs <- neighs(n)
          b <- satisfyFirst(SetUtils.pSet(neighs), satisfyMatches(te))
        } yield {
          logInfo(s"> satisfyShape(${n.show},${shape.show})=$b",-1)
          b
        }
      }
    }
  }

  private def showArcs(arcs: Set[Arc]) = arcs.map(_.show).mkString(",")
  private def showList[A: Show](ls: List[A]) = ls.map(_.show).mkString(",")

  def satisfyMatches(te: TripleExpr)(pair: (Set[Arc], Set[Arc])): Check[Boolean] = {
    val (matched,remainder) = pair
    logInfo(s"satisfyMatches(te=${te.show},matched=${showArcs(matched)},remainder=${showArcs(remainder)}",1)
    val outs = remainder.filter(_.path.isDirect)
    logInfo(s"Outs: ${outs.show}",0)
    for {
      matchables <- getMatchables(outs,te)
      unmatchables = outs diff matchables // matchables
      r <- {
        logInfo(s"matchables: ${matchables.show}",0)
        satisfyAll(
          List(
            matches(matched,te),
            notSatisfyMatchablesTCs(matchables, getTripleConstraints(te))
            // satisfyNot(matchesList(matchables, getTripleConstraints(te))
            // TODO: rest of conditions
          )
        )
      }
    } yield {
      logInfo(s"Tesult satisfyMatches(te=${te.show},matched=${showArcs(matched)},remainder=${showArcs(remainder)}=$r",-1)
      r
    }
  }

  def notSatisfyMatchablesTCs(arcs: Set[Arc], tcs: List[TripleConstraint]): Check[Boolean] = {
    logInfo(s"Not satisfy ${showArcs(arcs)} any of ${showList(tcs)}",1)
    for {r <- satisfyNot(
      satisfySome(arcs.toList.map(a =>
        satisfySome(tcs.map(tc =>
          matches(Set(a), tc))
        ))))
    } yield {
      logInfo(s"Result Not satisfy ${showArcs(arcs)} any of ${showList(tcs)}",-1)
      r
    }
  }

  def getTripleConstraints(te: TripleExpr): List[TripleConstraint] = te match {
    case tc: TripleConstraint => List(tc)
    case eo: EachOf => eo.expressions.map(e => getTripleConstraints(e)).flatten
    case oo: OneOf => oo.expressions.map(e => getTripleConstraints(e)).flatten
    case i: Inclusion => throw new Exception(s"getTripleConstraints of inclusion: $i")
    case e: Expr => List()
  }

  def getMatchables(arcs: Set[Arc], te: TripleExpr): Check[Set[Arc]] = te match {
    case tc: TripleConstraint => pure(arcs.filter(_.path.pred == tc.predicate))
    case eo: EachOf => {
      sequence(eo.expressions.map(te => getMatchables(arcs,te))).map(_.flatten.toSet)
    }
    case oo: OneOf => {
      sequence(oo.expressions.map(te => getMatchables(arcs,te))).map(_.flatten.toSet)
    } // oo.expressions.map(te => getMatchables(arcs,te)).toSet.flatten.sequence
    case e: Expr => pure(Set())
    case i: Inclusion => unimplemented(s"getMatchables inclusion")
  }

  def matches(matched: Set[Arc],
              te: TripleExpr
             ): Check[Boolean] = {
    logInfo(s"matches(matched=${showArcs(matched)},te=${te.show})",1)
    val r = te match {
      case oneOf: OneOf => matchesOneOf(matched, oneOf)
      case eachOf: EachOf => matchesEachOf(matched, eachOf)
      case tc: TripleConstraint => matchesTripleConstraint(matched, tc)
      case i: Inclusion => matchesInclusion(matched, i)
      case e: Expr => matchesExpr(matched, e)
    }
    for {
      b <- r
    } yield {
      logInfo(s"Result matches(matched=${showArcs(matched)},te=${te.show}=$b",-1)
      b
    }
  }

  def matchesOneOf(matched: Set[Arc], oneOf: OneOf): Check[Boolean] = {
    logInfo(s"matchesOneOf(matched=${showArcs(matched)},${oneOf.show})",1)
    for { b <- satisfySome(oneOf.expressions.map(te => matches(matched,te)))
    } yield {
      logInfo(s"Result of matchesOneOf(matched=${showArcs(matched)}, ${oneOf.show}=$b",-1)
      b
    }
  }

  def matchesEachOf(matched: Set[Arc], eachOf: EachOf): Check[Boolean] = {
    logInfo(s"matchesEachOf(matched=${showArcs(matched)},${eachOf.show})",0)
    satisfyFirst(
      SetUtils.partition(matched, eachOf.expressions.length),
      (ls: List[Set[Arc]]) => matchesList(ls, eachOf.expressions))
  }

  def matchesList(arcs: List[Set[Arc]], expressions: List[TripleExpr]): Check[Boolean] = {
    satisfyAll((arcs zip expressions).map{
      case (as,e) => matches(as,e) }
    )
  }

  def matchesTripleConstraint(matched: Set[Arc],
                              tc: TripleConstraint
                             ): Check[Boolean] =
   {
     logInfo(s"matchesTripleConstraint(${showArcs(matched)},${tc.show})",1)
     val r = matched.size match {
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
     for {
       b <- r
     } yield {
       logInfo(s"Result matchesTripleConstraint(${showArcs(matched)},${tc.show})=$b",-1)
       b
     }
   }

  def matchTripleConstraint(arc: Arc,
                            tc: TripleConstraint
                           ): Check[Boolean] = {
    logInfo(s"matchTripleConstraint(${arc.show},${tc.show})",1)
    val r = if (tc.direct) {
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
    for {
      b <- r
    } yield {
      logInfo(s"Result matchTripleConstraint(${arc.show},${tc.show})=$b",-1)
      b
    }
  }

  def matchesExpr(matched: Set[Arc], e: Expr): Check[Boolean] =
    unimplemented(s"matchesExpr $e")

  def matchesInclusion(matched: Set[Arc], i: Inclusion): Check[Boolean] =
    unimplemented(s"matchesInclusion $i")

  def neighs(n: RDFNode): Check[Set[Arc]] = for {
    rdf <- getRDF
    outTriples <- fromEither(rdf.triplesWithSubject(n))
    outArcs = outTriples.map(t => Arc(Direct(t.pred),t.obj))
    inTriples <- fromEither(rdf.triplesWithObject(n))
    inArcs = inTriples.map(t => Arc(Inverse(t.pred),t.obj))
  } yield {
    val allArcs = outArcs ++ inArcs
    logInfo(s"neighs($n): ${allArcs.map(_.show).mkString(",")}",0)
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
      case Left(e) => err(e)
      case Right(se) => pure(se)
    }
  } yield shape

  def getShapeFromShapeMapLabel(lbl: ShapeMapLabel): Check[ShapeExpr] = for {
    schema <- getSchema
    shape <- lbl match {
      case StartMapLabel => schema.start match {
        case None => err(s"Not found Start in schema")
        case Some(se) => pure(se)
      }
      case IriShapeMapLabel(iri) => getShape(IRILabel(iri))
      case BNodeShapeMapLabel(bn) => getShape(BNodeLabel(bn))
    }
  } yield shape

  def cnvInfo(info: Info[Evidence]): ShapeMapInfo =
    /* TOOD: add evidences...*/
    info match {
    case Conformant(es) => ShapeMapInfo(ConformantStatus, None, None)
    case NonConformant(es) => ShapeMapInfo(NonConformantStatus,None,None)
    case Unknown => ShapeMapInfo(Undefined, None, None)
  }

  def shapeTyping2ResultShapeMap(typing: ShapeTyping,
                                 nodesPrefixMap: PrefixMap,
                                 shapesPrefixMap: PrefixMap
                                ): ResultShapeMap = {
    val resultMap: Map[RDFNode,Map[ShapeMapLabel,ShapeMapInfo]] =
      typing.m.mapValues(valueMap => valueMap.mapValues(info => cnvInfo(info)).toMap).toMap
    ResultShapeMap(resultMap, nodesPrefixMap, shapesPrefixMap)
  }

}