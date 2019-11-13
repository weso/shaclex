package es.weso.shex.validator

import cats._
import data._
import implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.collection.Bag
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.{BagChecker, Empty, Rbe}
import es.weso.rbe.BagChecker._
import es.weso.utils.{SeqUtils, SetUtils}
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.PREFIXES._
import es.weso.shex.validator.Table._
import ShExChecker._
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMapLabel, _}
import es.weso.shex.actions.TestSemanticAction
import es.weso.shex.normalized._

import Function.tupled

/**
 * ShEx validator
 */
case class Validator(schema: Schema,
                     externalResolver: ExternalResolver = NoAction
                    ) extends ShowValidator(schema) with LazyLogging {

  type ShapeChecker     = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping
  type NodeChecker      = Attempt => RDFNode => CheckTyping
  type Neighs           = List[Arc]
  type Candidates       = List[Candidate]
  type NoCandidates     = List[Arc]
  type Bag_             = Bag[ConstraintRef]
  type Rbe_             = Rbe[ConstraintRef]
  type BagChecker_      = BagChecker[ConstraintRef]
  type ES[A]            = Either[ShExError, A]

  private lazy val `sh:targetNode` = sh + "targetNode"


  private lazy val ignoredPathsClosed: List[Path] =
      List(Inverse(`sh:targetNode`))

  private[validator] def checkTargetNodeDeclarations: CheckTyping =
      for {
        rdf        <- getRDF
        nodeLabels <- getTargetNodeDeclarations(rdf)
        ts <- checkAll(nodeLabels.map {
          case (node, label) => checkNodeLabel(node, label)
        })
        t <- combineTypings(ts)
      } yield t

  private[validator] def checkShapeMap(shapeMap: FixedShapeMap): CheckTyping = {
      checkNodesShapes(shapeMap)
  }

  private[validator] def getTargetNodeDeclarations(rdf: RDFReader): Check[List[(RDFNode, ShapeLabel)]] =
      for {
        ts <- fromEitherString(rdf.triplesWithPredicate(`sh:targetNode`))
        r  <- checkAll(ts.map(t => (t.obj, mkShapeLabel(t.subj))).toList.map(checkPair2nd))
      } yield r

  private[validator] def checkNodesShapes(fixedMap: FixedShapeMap): CheckTyping =
      for {
        ts <- checkAll(fixedMap.shapeMap.toList.map(tupled(checkNodeShapesMap)))
        t  <- combineTypings(ts)
      } yield t

  private[validator] def checkNodeShapeMapLabel(node: RDFNode,
                                                label: ShapeMapLabel,
                                                info: Info
                                               ): CheckTyping =
    info.status match {
        case Conformant =>
          label match {
            case StartMapLabel => checkNodeStart(node)
            case IRIMapLabel(_) | BNodeMapLabel(_) => {
              val lbl = mkLabel(label)
              checkNodeShapeLabel(node, lbl)
            }
          }
        case NonConformant =>
          label match {
            case StartMapLabel =>
              for {
                c <- checkNodeStart(node)
                check <- checkNotConformant(node,Start,c)
              } yield check
            case IRIMapLabel(_) | BNodeMapLabel(_) => {
              val lbl = mkLabel(label)
              for {
                c     <- checkNodeShapeLabel(node, lbl)
                check <- checkNotConformant(node, lbl, c)
              } yield check
            }
          }
        case Undefined =>
          errStr(s"Cannot check $node agains undefined status")
    }

  private def mkLabel(label: ShapeMapLabel): ShapeLabel = label match {
    case StartMapLabel => Start
    case IRIMapLabel(iri) => IRILabel(iri)
    case BNodeMapLabel(b) => BNodeLabel(b)
  }

  private def checkNotConformant(node: RDFNode,
                                 label: ShapeLabel,
                                 c: ShapeTyping): CheckTyping =
    if (c.hasNoType(node,label)) ok(c)
    else errStr(s"Node $node should not conform to $label but it does")

  private def checkLabelInfo(node: RDFNode)(pair: (ShapeMapLabel,Info)): CheckTyping = {
    val (label,info) = pair
    checkNodeShapeMapLabel(node,label,info)
  }

  private[validator] def checkNodeShapesMap(node: RDFNode,
                                            shapesMap: Map[ShapeMapLabel, Info]
                                           ): CheckTyping = {
  for {
      ts <- checkAll(shapesMap.map(checkLabelInfo(node)).toList)
      t <- combineTypings(ts)
    } yield t
  }

  private[validator] def mkShapeLabel(n: RDFNode): Check[ShapeLabel] = {
    n match {
      case i: IRI => ok(IRILabel(i))
      case b: BNode => ok(BNodeLabel(b))
      case _ => {
        errStr(s"mkShapeLabel: Node ${n.show} can't be a shape")
      }
    }
  }

  private[validator] def getShape(label: ShapeLabel): Check[ShapeExpr] =
    schema.getShape(label) match {
      case Left(e) => errStr[ShapeExpr](e)
      case Right(shape) => ok(shape)
    }

  private[validator] def checkNodeShapeLabel(node: RDFNode, shape: ShapeLabel): CheckTyping = {
    cond(getShapeLabel(shape),
      (shapeLabel: ShapeLabel) => checkNodeLabel(node, shapeLabel),
      err => for {
      t <- getTyping
    } yield t.addNotEvidence(node, ShapeType(ShapeExpr.fail, Some(shape), schema), err))
  }

  private[validator] def checkNodeStart(node: RDFNode): CheckTyping = {
    schema.start match {
      case None => err(NoStart(node))
      case Some(shape) => {
        logger.debug(s"nodeStart. Node: ${node.show}")
        val shapeType = ShapeType(shape, Some(Start), schema)
        val attempt = Attempt(NodeShape(node, shapeType), None)
        runLocalSafeTyping(checkNodeShapeExpr(attempt, node, shape),_.addType(node,shapeType),
          (err, t) => {
            t.addNotEvidence(node, shapeType, err)
          })
      }
    }
  }

  private[validator] def getShapeLabel(label: ShapeLabel): Check[ShapeLabel] = {
    if (schema.labels contains label) ok(label)
    else err(LabelNotFound(label,schema.labels))
  }

  private[validator] def checkNodeLabelSafe(node: RDFNode,
                                            label: ShapeLabel,
                                            shape: ShapeExpr): CheckTyping = {
    val shapeType = ShapeType(shape, Some(label), schema)
    val attempt = Attempt(NodeShape(node, shapeType), None)
    for {
      _ <- getTyping
      t <- {
        runLocalSafeTyping(
          bind(
            checkOptSemActs(node,schema.startActs),
            checkNodeShapeExpr(attempt, node, shape)
          ),
          _.addType(node, shapeType),
          (err, t) => {
            t.addNotEvidence(node, shapeType, err)
          })
      }
    } yield {
      t
    }
  }

  private[validator] def checkNodeLabel(node: RDFNode, label: ShapeLabel): CheckTyping = {

    def addNot(typing: ShapeTyping)(err: ShExError): CheckTyping = {
      val shapeType = ShapeType(ShapeExpr.fail, Some(label), schema)
      ok(typing.addNotEvidence(node, shapeType, err))
    }

    for {
      typing <- getTyping
      newTyping <- if (typing.hasInfoAbout(node, label)) {
        ok(typing)
      } else
        cond(getShape(label),
             (shape: ShapeExpr) => checkNodeLabelSafe(node, label, shape),
             addNot(typing)
        )
    } yield newTyping
  }

  private[validator] def checkNodeShapeExpr(attempt: Attempt,
                                            node: RDFNode,
                                            s: ShapeExpr): CheckTyping = {
    s match {
      case so: ShapeOr => checkOr(attempt, node, so.shapeExprs)
      case sa: ShapeAnd => checkAnd(attempt, node, sa.shapeExprs)
      case sn: ShapeNot => checkNot(attempt, node, sn.shapeExpr)
      case nc: NodeConstraint => checkNodeConstraint(attempt, node, nc)
      case s: Shape => checkShape(attempt, node, s)
      case sr: ShapeRef => checkRef(attempt, node, sr.reference)
      case se: ShapeExternal => {
        for {
          externalShape <- se.id match {
            case None        =>
              errStr(s"No label in external shape")
            case Some(label) =>
              fromEitherString(externalResolver.getShapeExpr(label, se.annotations))
          }
          newAttempt = Attempt(NodeShape(node,ShapeType(externalShape,se.id,schema)),attempt.path)
          t <- checkNodeShapeExpr(newAttempt,node,externalShape)
        } yield t
      }
    }
  }

  private[validator] def checkAnd(
    attempt: Attempt,
    node: RDFNode,
    ses: List[ShapeExpr]): CheckTyping = for {
    ts <- checkAll(ses.map(se => checkNodeShapeExpr(attempt, node, se)))
    t <- combineTypings(ts)
  } yield t

  private[validator] def checkOr(
    attempt: Attempt,
    node: RDFNode,
    ses: List[ShapeExpr]): CheckTyping = {
    val vs = ses.map(se => checkNodeShapeExpr(attempt, node, se))
    for {
      t1 <- checkSome(
        vs,
        StringError(s"None of the alternatives of OR(${ses.map(_.showPrefixMap(schema.prefixMap)).mkString(",")}) is valid for node ${node.show}"))
      t2 <- addEvidence(attempt.nodeShape, s"${node.show} passes OR")
      t3 <- combineTypings(Seq(t1, t2))
    } yield t3
  }

  private[validator] def checkNot(
    attempt: Attempt,
    node: RDFNode,
    s: ShapeExpr): CheckTyping = {
    val parentShape = attempt.nodeShape.shape
    val check: CheckTyping = checkNodeShapeExpr(attempt, node, s)
    val handleError: ShExError => Check[ShapeTyping] = e => for {
      t1 <- addNotEvidence(NodeShape(node, ShapeType(s, None, schema)), e,
        s"${node.show} does not satisfy ${s.show}. Negation declared in ${parentShape.show}. Error: $e")
      t2 <- addEvidence(attempt.nodeShape, s"${node.show} satisfies not(${{ s.show }})")
      t <- combineTypings(List(t1, t2))
    } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      errStr(s"Failed NOT(${s.show}) because node ${node.show} satisfies it")
    cond(check, handleNotError, handleError)
  }

  private[validator] def checkRef(
    attempt: Attempt,
    node: RDFNode,
    ref: ShapeLabel): CheckTyping = for {
    t <- checkNodeLabel(node, ref)
    _ <- if (t.hasNoType(node, ref)) {
      t.getTypingResult(node, ref) match {
        case None => errStr(s"Node ${node.show} has no shape ${ref.show}. Attempt: $attempt")
        case Some(tr) => tr.getErrors match {
          case None => errStr(s"Node ${node.show} has no shape ${ref.show}\nReason typing result ${tr.show} with no errors")
          case Some(es) => errStr(s"Node ${node.show} has no shape ${ref.show}\nErrors: ${es.map(_.show).mkString("\n")}")
        }
      }
    } else ok(())
  } yield t

  private[validator] def checkNodeConstraint(
    attempt: Attempt,
    node: RDFNode,
    s: NodeConstraint): CheckTyping =
    for {
      t1 <- optCheck(s.nodeKind, checkNodeKind(attempt, node), getTyping)
      t2 <- optCheck(s.values, checkValues(attempt, node), getTyping)
      t3 <- optCheck(s.datatype, checkDatatype(attempt, node), getTyping)
      t4 <- checkXsFacets(attempt, node)(s.xsFacets)
      t <- combineTypings(List(t1, t2, t3, t4))
    } yield {
      t
    }

  private[validator] def checkValues(attempt: Attempt, node: RDFNode)(values: List[ValueSetValue]): CheckTyping = {
    val cs: List[CheckTyping] =
      values.map(v => ValueChecker(schema).checkValue(attempt, node)(v))
    checkSome(cs,
      StringError(s"${node.show} does not belong to [${values.map(_.show).mkString(",")}]")
    )
  }

  private[validator] def checkDatatype(attempt: Attempt, node: RDFNode)(datatype: IRI): CheckTyping = for {
    rdf <- getRDF
    check <- rdf.checkDatatype(node, datatype) match {
      case Left(s) => errStr(s"${attempt.show}\n${node.show} does not have datatype ${datatype.show}\nDetails: $s")
      case Right(false) => errStr(s"${attempt.show}\n${node.show} does not have datatype ${datatype.show}")
      case Right(true) => addEvidence(attempt.nodeShape, s"${node.show} has datatype ${datatype.show}")
    }
  } yield check

  private[validator] def checkXsFacets(attempt: Attempt, node: RDFNode)(xsFacets: List[XsFacet]): CheckTyping = {
    if (xsFacets.isEmpty) getTyping
    else for {
      rdf <- getRDF
      t <- FacetChecker(schema,rdf).checkFacets(attempt, node)(xsFacets)
    } yield t
  }

  private[validator] def checkNodeKind(attempt: Attempt, node: RDFNode)(nk: NodeKind): CheckTyping = {
    nk match {
      case IRIKind =>
        checkCond(node.isIRI, attempt,
          StringError(s"${node.show} is not an IRI"), s"${node.show} is an IRI")
      case BNodeKind =>
        checkCond(node.isBNode, attempt,
          StringError(s"${node.show} is not a BlankNode"), s"${node.show} is a BlankNode")
      case NonLiteralKind =>
        checkCond(!node.isLiteral, attempt,
          StringError(s"${node.show} is a literal but should be a NonLiteral"),
          s"${node.show} is NonLiteral")
      case LiteralKind =>
        checkCond(node.isLiteral, attempt,
          StringError(s"${node.show} is not an Literal"),
          s"${node.show} is a Literal")
    }
  }

  private[validator] def checkShape(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping =
    s._extends match {
      case None => checkShapeBase(attempt,node,s)
      case Some(es) => checkShapeExtendLs(attempt, node, s, es)
    }

  private[validator] def checkShapeExtendLs(attempt: Attempt, node: RDFNode, s: Shape, es: List[ShapeLabel]): CheckTyping = {
    es match {
      case Nil => checkShapeBase(attempt,node,s)
      case e :: Nil => checkShapeExtend(attempt, node, s, e)
      case _ => errStr(s"Multiple inheritance not supported yet")
    }
  }

  private[validator] def checkShapeExtend(attempt: Attempt,
                                          node: RDFNode,
                                          s: Shape,
                                          baseLabel: ShapeLabel
                                         ): CheckTyping = for {
    base <- getShape(baseLabel)
    paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
    // _ <- { println(s"checkShapeExtend(node=$node,shape=${s.show},base=$baseLabel). \npaths=$paths") ; ok(()) }
    neighs <- getNeighPaths(node, paths)
    partitions = SetUtils.pSet(neighs.toSet)
    _ <- checkSomeFlag(partitions,checkPartition(base,s,attempt,node), noPartition(node, neighs))
    typing <- getTyping
  } yield typing


  private[validator] def checkPartition(base: ShapeExpr,
                                        s: Shape,
                                        attempt: Attempt,
                                        node: RDFNode
                                       )(pair: (Set[Arc],Set[Arc])): Check[(ShapeTyping,Boolean)] = {
    val (neighs1,neighs2) = pair
    // println(s"Checking partition ($neighs1,$neighs2)\n$neighs1 with ${base.show}\nand\n$neighs2 with ${s.show}")
    (for {
      pair <- checkNeighsShapeExpr(attempt, node, neighs1.toList, base)
      (typing1,flag) = pair
      // _ <- { println(s"Typing1: $typing1"); ok(()) }
      typing2 <- checkNeighsShape(attempt,node,neighs2.toList,s)
      // _ <- { println(s"Typing2: $typing2"); ok(()) }
    } yield (typing2,true)) orElse
      (for {
      // _ <- {println(s"partition ($neighs1,$neighs2) failed"); ok(()) }
      t <- getTyping
    } yield (t,false))
  }

/*  private def orElseDebug[A](c1: Check[A], c2:Check[A]): Check[A] = {
    c1.orElse(c2)
  } */

  private[validator] def noPartition(node: RDFNode, neighs: Neighs): Check[(ShapeTyping,Boolean)] =
    errStr(s"No partition of $neighs conforms. Node: $node")

  private[validator] def checkNeighsShapeExpr(attempt: Attempt, node: RDFNode, neighs: Neighs, se: ShapeExpr): Check[(ShapeTyping,Boolean)] =
    se match {
    case s: Shape => (for {
      t <- checkNeighsShape(attempt,node,neighs,s)
      // _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} passed with $t") ; ok(())}
    } yield (t,true)) orElse {
      for {
      // _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} failed") ; ok(())}
      t <- getTyping
      } yield (t,false)
    }
    case _ => {
      // println(s"Not implemented yet extends with a non shape base: $se")
      errStr(s"Not implemented yet extends with a non shape base: $se")
    }
  }

  private[validator] def checkNeighsShape(attempt: Attempt, node: RDFNode, neighs: Neighs, s: Shape): CheckTyping =
    if (s.hasRepeatedProperties(schema))
     checkNeighsShapeWithTable(attempt,node,neighs,s) else {
     // TODO
     checkNeighsShapeWithTable(attempt,node,neighs,s)
  }

  private[validator] def checkNeighsShapeWithTable(attempt: Attempt, node: RDFNode, neighs: Neighs, s: Shape): CheckTyping =
    for {
    tableRbe <- mkTable(s.expression, s.extra.getOrElse(List()))
    (cTable, rbe) = tableRbe
    bagChecker = IntervalChecker(rbe)
    csRest <- {
      calculateCandidates(neighs, cTable)
    }
    (candidates, rest) = csRest
    _ <- checkRests(rest, s.extraPaths, s.isClosed, ignoredPathsClosed)
    paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
    _ <- { if (s.isClosed) {checkNoStrangeProperties(node, paths)} else ok(())      }
    typing <- {
    //  println(s"Before checkCandidates: ${candidates.map(_.show).mkString(",")}\nTable:${cTable.show}\n")
      checkCandidates(attempt, bagChecker, cTable)(candidates)
    }
    // _ <- {println(s"After checkCandidates: $typing"); ok(()) }
    _ <- {
      // println(s"checkOptSemActs: ${s.actions}")
      checkOptSemActs(node,s.actions)
    }
  } yield {
    // println(s"End of checkShape(attempt=${attempt.show},node=${node.show},shape=${s.show})=${typing.show}")
    typing
  }

  private def getPaths(s: Shape): Check[List[Path]] =
    fromEitherString(s.paths(schema))

  private[validator] def checkShapeBase(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping = {
    //println(s"checkShapeBase: $node, $s. Normalized?: ${s.isNormalized(schema)}")
    s match {
      case _ if s.isEmpty => addEvidence(attempt.nodeShape, s"Node $node matched empty shape")
      case _ if s.isFlatShape(schema) => for {
        flatShape <- fromEitherString(s.flattenShape(schema))
        typing <- checkFlatShape(attempt, node, flatShape)
      } yield typing
      case _ => for {
        paths <- {
          getPaths(s)
        }
        neighs <- {
          //println(s"Paths: $paths")
          getNeighPaths(node, paths)
        }
        typing <- {
          //println(s"Neighs: $neighs")
          checkNeighsShape(attempt, node, neighs, s)
        }
      } yield typing
    }
  }

  private[validator] def checkFlatShape(attempt: Attempt,
                                        node: RDFNode,
                                        s: FlatShape
                                       ): CheckTyping = {
    val zero = getTyping
    def cmb(ct: CheckTyping, slot: (Path, Constraint)): CheckTyping = {
      val (path, constraint) = slot
      //println(s"Checking constraint: path: $path\nConstraint: $constraint")
      for {
        typing1 <- ct
        typing2 <- checkConstraint(attempt, node, path, constraint)
        typing <- combineTypings(List(typing1,typing2))
      } yield {
        //println(s"Typing: ${typing.getMap}")
        typing
      }
    }
    for {
      extra <- extraPreds(node,s.preds)
      _ <- {
        //println(s"Extra preds: $extra. Closed? ${s.closed}")
        ok(extra)
      }
      typing <- if (s.closed && extra.nonEmpty) {
        err(ClosedButExtraPreds(extra))
      } else s.slots.foldLeft(zero)(cmb)
    } yield typing
  }

  private def getExistingPredicates(node: RDFNode): Check[Set[IRI]] = for {
    rdf <- getRDF
    ps <- fromEitherString(rdf.triplesWithSubject(node))
  } yield ps.map(_.pred)

  // Returns the list of paths that are different from a given list
  private def extraPreds(node: RDFNode, preds: Set[IRI]): Check[Set[IRI]] = for {
    existingPreds <-  getExistingPredicates(node)
  } yield existingPreds -- preds

  private def checkConstraint(attempt: Attempt, node: RDFNode, path: Path, constraint: Constraint): CheckTyping = for {
   values <- getValuesPath(node,path)
   typing <- checkValuesConstraint(values, constraint, node, path, attempt)
  } yield typing

  private def getValuesPath(node: RDFNode, path: Path): Check[Set[RDFNode]] = for {
    rdf <- getRDF
    nodes <- fromEitherString(path.getValues(node,rdf))
  } yield nodes

  // We assume that the shape has no reference to other shapes
  private def checkValuesConstraint(values: Set[RDFNode], constraint: Constraint, node: RDFNode, path: Path, attempt: Attempt): CheckTyping = {
    val card: Cardinality = constraint.card
    constraint.shape match {
        case None =>
          if (card.contains(values.size)) addEvidence(attempt.nodeShape, s"Number of values fits $card")
          else err(ErrCardinality(attempt, node, path, values.size, card))
        case Some(se) => if (constraint.hasExtra) {
          for {
            rdf <- getRDF
            t <- {
              val (passed, notPassed) = values.map(checkNodeShapeExprBasic(_, se, rdf)).partition(_.isRight)
              if (card.contains(passed.size)) {
                addEvidence(attempt.nodeShape, s"Number of values for ${node.show} with ${path.show} that satisfy ${constraint.shape} = ${passed.size} matches cardinality ${constraint.card}")
              } else {
                err(ErrCardinalityWithExtra(attempt, node, path, passed.size, notPassed.size, card))
              }
            }
          } yield t
        }
        else for {
          rdf <- getRDF
          t <- {
            if (constraint.card.contains(values.size)) {
              val (notPassed, passed) =
                values.map(v => (v, checkNodeShapeExprBasic(v, se, rdf))).partitionMap(mapFun)
              // println(s"checkValuesConstraint: \nPassed: $passed\nNot passed: $notPassed")
              if (notPassed.isEmpty) {
                addEvidence(attempt.nodeShape, s"${node.show} passed ${constraint.show} for path ${path.show}")
              } else
                err(ValuesNotPassed(attempt, node, path, passed.size, notPassed))
            } else err(ErrCardinality(attempt, node, path, values.size, card))
        }
    } yield t
   }
  }

  private def mapFun(v: (RDFNode,Either[String,String])
                    ): Either[(RDFNode,String), (RDFNode,String)] = {
    val (node, either) = v
    either match {
      case Left(s) => Left((node,s))
      case Right(s) => Right((node,s))
    }
  }

  private def checkNodeShapeExprBasic(node: RDFNode,
                                      se: ShapeExpr,
                                      rdf: RDFReader
                                     ): Either[String,String] =
  se match {
    case sa: ShapeAnd => cmb(sa.shapeExprs.map(checkNodeShapeExprBasic(node,_, rdf)))
    case so: ShapeOr => cmb(so.shapeExprs.map(checkNodeShapeExprBasic(node,_, rdf)))
    case sn: ShapeNot =>
      if (checkNodeShapeExprBasic(node,sn.shapeExpr,rdf).isLeft)
        s"Passes negation".asRight[String]
      else s"Doesn't pass negation".asLeft[String]
    case _: ShapeRef => Left("Internal error. A normalized ShapeExpr cannot have references ")
    case s: Shape if s.isEmpty => Right(s"$node matches empty shape")
    case s: Shape =>
      // checkShapeBase(Attempt(NodeShape(node, ShapeType(s,s.id, schema)),None), node, s)
      Left(s"Not implemented yet")
    case _: ShapeExternal => Left(s"Still don't know what to do with external shapes")
    case nk: NodeConstraint => NodeConstraintChecker(schema,rdf).nodeConstraintChecker(node,nk)
  }

  private def cmb(els: List[Either[String,String]]): Either[String,String] = {
    els.sequence.map(_.mkString("\n"))
  }

  private def checkNoStrangeProperties(node: RDFNode, paths: List[Path]): Check[Unit] = for {
    rdf <- getRDF
    s <- getNotAllowedPredicates(node, paths)
    check <- if (s.isEmpty) ok(())
             else errStr(s"Closed shape does not allow properties: ${s.map(_.show).mkString(",")}")
  } yield check

  private[validator] def checkOptSemActs(node: RDFNode, maybeActs: Option[List[SemAct]]): Check[Unit] =
    maybeActs match {
      case None => ok(())
      case Some(as) => checkSemActs(node,as)
  }

  private[validator] def checkSemActs(node: RDFNode,
                                      as: List[SemAct]
                                     ): Check[Unit] = for {
    _ <- checkAll(as.map(checkSemAct(node,_)))
  } yield ()

  private[validator] def checkSemAct(node: RDFNode,
                                     a: SemAct
                                    ): Check[Unit] = for {
    rdf <- getRDF
    _ <- runAction(a.name,a.code,node,rdf)
  } yield ()

  private[validator] def runAction(name: IRI, code: Option[String], node: RDFNode, rdf: RDFReader): Check[Unit] = {
    // println(s"Semantic action: $name/$code")
    for {
      _ <- name match {
        case TestSemanticAction.`iri` => {
          TestSemanticAction.runAction(code.getOrElse(""), node, rdf).fold(e => errStr(e), _ => ok(()))
        }
        case _ => {
          logger.info(s"Unsupported semantic action processor: $name")
          addLog(List(Action(name, code)))
        }
      }
    } yield ()
  }

  private[validator] def checkRests(rests: List[Arc],
                                    extras: List[Path],
                                    isClosed: Boolean,
                                    ignoredPathsClosed: List[Path]): Check[Unit] = {
    val zero: Either[String,Unit] = Right(())
    def combine(step: Either[String,Unit],current: Either[String,Unit]): Either[String,Unit] =
      (step,current) match {
       case (Left(str1),_) => Left(str1)
       case (_, Left(str2)) => Left(str2)
       case (Right(()),Right(())) => Right(())
    }
    val ts: List[Either[String,Unit]] = rests.map(checkRest(_, extras, isClosed, ignoredPathsClosed))
    val r: Either[String, Unit] = ts.foldLeft(zero)(combine)
    r.fold(e => errStr(e), _ => ok(()))
  }

  private[validator] def checkRest(rest: Arc,
                                   extras: List[Path],
                                   isClosed: Boolean,
                                   ignoredPathsClosed: List[Path]
                                  ): Either[String,Unit] = {
    val restPath = rest.path
	  // Ignore extra predicates if they are inverse
    if (isClosed && restPath.isDirect) {
      // TODO: Review if the extra.contains(restpath) check is necessary
      // Extra has been implemented as a negation
      if (ignoredPathsClosed.contains(restPath) || extras.contains(restPath)) {
        Right(())
      } else {
        Left(s"Closed shape. But rest ${restPath.show} is not in ${ignoredPathsClosed.map(_.show).mkString(",")} or ${extras.map(_.show).mkString(",")}")
      }
    } else Right(())
  }

  private[validator] def mkTable(maybeTe: Option[TripleExpr],
                                 extra: List[IRI]
                                ): Check[(CTable, Rbe_)] = {
    maybeTe match {
      case None => ok((CTable.empty, Empty))
      case Some(te) => fromEitherString(
        for {
         tem <- schema.eitherResolvedTripleExprMap
         pair <- CTable.mkTable(te,extra,tem.getOrElse(Map()))
        } yield pair
      )
    }
  }

  /**
   * Calculates the sequence of candidates
   * Example: Neighs (p,x1),(p,x2),(q,x2),(r,x3)
   *   Table: { constraints: C1 -> IRI, C2 -> ., paths: p -> List(C1,C2), q -> C1 }
   *   Result: x1
   * @param neighs
   * @param table
   * @return a tuple (cs,rs) where cs is the list of candidates and rs is the nodes that didn't match any
   */
  private[validator] def calculateCandidates(
    neighs: Neighs,
    table: CTable): Check[(Candidates, NoCandidates)] = {
    val candidates = table.neighs2Candidates(neighs)
    val (cs, rs) = candidates.partition(matchable)
    ok((cs, rs.map(_.arc)))
  }

  private def matchable(c: Candidate): Boolean = {
    c.crefs.nonEmpty
  }

  private[validator] def showCandidateLines(cs: List[CandidateLine]): String = {
    cs.length match {
      case 0 => "No candidate lines"
      case 1 => s"One candidate line\n${cs.head.show}"
      case _ => cs.map(_.show).mkString("\n")
    }
  }

  private[validator] def checkCandidates(
    attempt: Attempt,
    bagChecker: BagChecker_,
    table: CTable)(cs: Candidates): CheckTyping = {
    // println(s"checkCandidates(...)")
    val as: List[CandidateLine] =
      SeqUtils.transpose(cs.map(c => (c.arc,c.crefs))).
        map(CandidateLine(_))

    // println(s"Candidate lines: $as, ${as.length}")
    as.length match {
      case 1 => { // Deterministic
        checkCandidateLine(attempt, bagChecker, table)(as.head)
      }
      case 0 => {
        errStr(s"${attempt.show} Empty list of candidates")
      }
      case n => {
        // println(s"Non deterministic")
        val checks: List[CheckTyping] =
          as.map(checkCandidateLine(attempt, bagChecker, table)(_))
        checkSome(checks,{
          // println(s"None of the candidates match")
          StringError(
            s"""|None of the candidates matched. Attempt: ${attempt.show}
                |Bag: ${bagChecker.show}
                |Candidate lines:${showCandidateLines(as)}
                |""".stripMargin
        )})

      }
    }
  }

  private[validator] def showListCandidateLine(ls: List[CandidateLine]): String = {
    ls.map(_.show).mkString("\n")
  }

  private[validator] def checkCandidateLine(attempt: Attempt,
                                            bagChecker: BagChecker_,
                                            table: CTable
                                           )(cl: CandidateLine): CheckTyping = {
    // println(s"checkCandidateLine: ${cl}")
    // println(s"Table: $table")
    val bag = cl.mkBag
    bagChecker.check(bag, false).fold(
      e => {
        // println(s"Does not match RBE. ${bag} with ${bagChecker.show}")
        errStr(s"${attempt.show} Candidate line ${cl.show} which corresponds to ${bag} does not match ${Rbe.show(
          bagChecker.rbe)}\nTable:${table.show}\nErr: $e")
      },
      bag => {
        // println(s"Matches RBE...")
        val nodeConstraints = cl.nodeConstraints(table)
        val checkNodeConstraints: List[CheckTyping] =
          nodeConstraints.map {
            case (node, pair) => {
              val (shapeExpr, maybeSemActs) = pair
              // println(s"Checking $node with $shapeExpr")
              for {
              t <- checkNodeShapeExpr(attempt, node, shapeExpr)
              _ <- checkOptSemActs(node,maybeSemActs)
            } yield t
          }}
        for {
          typing <- getTyping
          ts <- checkAll(checkNodeConstraints)
          t <- combineTypings(typing :: ts)
        } yield {
          t
        }
      }
    )
  }

  private[validator] def getNeighs(node: RDFNode): Check[Neighs] =
  for {
    rdf <- getRDF
    outTriples <- fromEitherString(rdf.triplesWithSubject(node))
    outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
    inTriples <- fromEitherString(rdf.triplesWithObject(node))
    incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
  } yield {
    val neighs = outgoing ++ incoming
    neighs
  }

  private[validator] def getNeighPaths(node: RDFNode, paths: List[Path]): Check[Neighs] = {
    val outgoingPredicates = paths.collect { case Direct(p) => p }
    for {
      rdf        <- getRDF
      outTriples <- fromEitherString(rdf.triplesWithSubjectPredicates(node, outgoingPredicates))
      // _ <- { println(s"getNeighPaths\ntriplesWithSubjectPredicates($node, $outgoingPredicates): Outtriples: $outTriples\nRDF: ${rdf.serialize("TURTLE")}\nNode: $node\nPreds:$outgoingPredicates"); ok(()) }
      outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
      inTriples <- fromEitherString(rdf.triplesWithObject(node))
      incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
    } yield {
      val neighs = outgoing ++ incoming
      neighs
    }
  }

  private[validator] def getNotAllowedPredicates(node: RDFNode, paths: List[Path]): Check[Set[IRI]] = for {
    rdf <- getRDF
    ts <- fromEitherString(rdf.triplesWithSubject(node))
  } yield {
    val allowedPreds = paths.collect { case Direct(p) => p }
    ts.collect {
      case s if !(allowedPreds contains s.pred) => s.pred
    }
  }



  lazy val emptyTyping: ShapeTyping = Monoid[ShapeTyping].empty

  def validateNodeDecls(rdf: RDFReader): Result = {
    runValidator(checkTargetNodeDeclarations, rdf)
  }

  def validateNodeShape(rdf: RDFReader, node: IRI, shape: String): Result = {
    ShapeLabel.fromString(shape).fold(
      e => strErr(s"Can not obtain label from $shape"),
      label => runValidator(checkNodeShapeLabel(node, label), rdf)
    )
  }

  private def strErr(str: String): Result = Result(Left(StringError(str)))

  def validateNodeStart(rdf: RDFReader, node: IRI): Result = {
    runValidator(checkNodeStart(node), rdf)
  }

  def validateShapeMap(rdf: RDFReader, shapeMap: FixedShapeMap): Result = {
    runValidator(checkShapeMap(shapeMap), rdf)
  }

  def runValidator(chk: Check[ShapeTyping], rdf: RDFReader): Result = {
    cnvResult(runCheck(chk, rdf),rdf)
  }

  private def cnvResult(r: CheckResult[ShExError, ShapeTyping, Log],
                rdf: RDFReader): Result = Result(
   for {
    shapeTyping <- r.toEither
    result <- shapeTyping.toShapeMap(rdf.getPrefixMap, schema.prefixMap).leftMap(StringError(_))
   } yield result
  )

}

object Validator {

  def empty = Validator(schema = Schema.empty)

  type Result[A] = Either[NonEmptyList[ShExError], List[(A, Evidences)]]

  def isOK[A](r: Result[A]): Boolean =
    r.isRight && r.toList.isEmpty == false

  def validate(schema: Schema, fixedShapeMap: FixedShapeMap, rdf: RDFReader): Either[String, ResultShapeMap] = {
    val validator = Validator(schema)
    validator.validateShapeMap(rdf, fixedShapeMap).toEitherS
  }

}

