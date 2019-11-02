package es.weso.shacl.validator

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.path.{PredicatePath, SHACLPath}
import es.weso.shacl._
import es.weso.utils._
import es.weso.shacl.showShacl._
import SHACLChecker._
import es.weso.rdf.operations.Comparisons
import es.weso.shacl.report.{AbstractResult, MsgError, Severity, ValidationResult}
import es.weso.shacl.report.ValidationResult._
import es.weso.rdf.operations.Comparisons._
import es.weso.rdf.triples.RDFTriple

/**
 * This validator is implemented directly in Scala using cats library
 */

case class Validator(schema: Schema) extends LazyLogging {

  /**
   * Return all targetNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def targetNodes: Seq[(RDFNode, Shape)] = {
    schema.targetNodeShapes
  }

  def runCheck[A: Show](c: Check[A], rdf: RDFReader): CheckResult[AbstractResult, A, Log] = {
    val initial: ShapeTyping = ShapeTyping.empty
    val r = run(c)(rdf)(initial)
    CheckResult(r)
  }

  /**
   * Checks if all nodes/shapes are valid in a schema
   * Fails if any of them is not correct
   */
  def checkSchemaAll: CheckTyping = {
    val shapes = schema.shapes.to(LazyList)
    checkAllTyping(shapes, shapeChecker)
  }

  def shapeChecker: ShapeChecker = shape => {
    logger.debug(s"Checking shape: ${shape.showId}")
    for {
      _ <- addLogMsg(s"Checking shape ${shape.showId}")
      r <- checkSequenceTyping(
        List(checkTargetNodes(shape.targetNodes)(shape),
             checkTargetClasses(shape.targetClasses)(shape),
             checkTargetSubjectsOf(shape.targetSubjectsOf)(shape),
             checkTargetObjectsOf(shape.targetObjectsOf)(shape)))
    } yield r
  }

  def checkTargetNodes(nodes: Seq[RDFNode]): ShapeChecker = shape => {
    logger.debug(s"Target nodes of ${shape.showId} = ${nodes.mkString(",")}")
    def chk(n: RDFNode): CheckTyping = nodeShape(n,shape)
//    val nodesShapes = nodes.map(n => nodeShape(n, shape)).toStream
    for {
      // rdf <- getRDF
      _ <- addLogMsg(s"Checking targetNode declarations for shape ${shape.showId}. Nodes: ${nodes}")
      r <- checkAllTyping(nodes.to(LazyList), chk)
    } yield {
      r
    }
  }

  def checkTargetClasses(classes: Seq[RDFNode]): ShapeChecker = shape => {
    def chk(n: RDFNode): CheckTyping = nodeShape(n,shape)
    logger.debug(s"Target classes of ${shape.showId} = ${classes.map(_.show).mkString(",")}")
    for {
      rdf <- getRDF
      nss <- sequence(classes.map(findNodesInClass(_, rdf)).toList)
      nodes = nss.flatten
      r <- checkAllTyping(nodes.to(LazyList), chk)
    } yield r
  }

  def getTriplesWithPredicate(p: IRI,
                              rdf: RDFReader
                             ): Check[Set[RDFTriple]] =
    fromEither(rdf.triplesWithPredicate(p).leftMap(MsgError(_)))

  def checkTargetSubjectsOf(preds: Seq[IRI]): ShapeChecker = shape => {
    def chk(n: RDFNode): CheckTyping = nodeShape(n,shape)
    for {
      rdf <- getRDF
      ts <- sequence(preds.map(getTriplesWithPredicate(_,rdf)).toList)
      subjects = ts.flatten.map(_.subj)
      r <- checkAllTyping(subjects.to(LazyList), chk)
    } yield r
  }

  private def checkTargetObjectsOf(preds: Seq[IRI]): ShapeChecker = shape => {
    def chk(n: RDFNode): CheckTyping = nodeShape(n,shape)
    for {
      rdf <- getRDF
      ts <- sequence(preds.map(getTriplesWithPredicate(_,rdf)).toList)
      objects = ts.flatten.map(_.obj)
      r <- checkAllTyping(objects.to(LazyList),chk)
    } yield r
  }

  def findNodesInClass(cls: RDFNode, rdf: RDFReader): Check[List[RDFNode]] =
    fromEither(rdf.getSHACLInstances(cls).map(_.toList).leftMap(MsgError(_)))

  private def nodeShapeRef(node: RDFNode, shapeRef: RefNode, attempt: Attempt): CheckTyping = for {
    rdf <- getRDF
    shape <- getShapeRef(shapeRef, attempt, node)
    t <- nodeShape(node, shape)
  } yield t

  def nodeShape(node: RDFNode, shape: Shape): CheckTyping = shape match {
    case ns: NodeShape => nodeNodeShape(node, ns)
    case ps: PropertyShape => nodePropertyShape(node,ps)
  }

  private def getSeverity(s: Shape): Severity =
    s.severity.getOrElse(Severity.defaultSeverity)

  def nodeNodeShape(node: RDFNode, ns: NodeShape): CheckTyping = {
    logger.debug(s"Node $node - NodeShape ${ns.showId}")
    logger.debug(s"Node shape is deactivated? (${ns.deactivated})")
    val attempt = Attempt(node, RefNode(ns.id), ns.message, getSeverity(ns), None)
    for {
        t0 <- getTyping
        t <- runLocal(checkNodeShape(ns)(attempt)(node), _.addType(node, ns))
      } yield {
        val r =
          if (t._2) t
          else (t._1.addNotEvidence(node, ns, shapesFailed(node, ns, Set(),
          attempt,
          s"$node does not have nodeShape ${ns.showId} because some shapes failed.")), false)
        logger.debug(s"Result of node $node - NodeShape ${ns.showId})\n${showResult(r)}")
        r
      }
  }

  def nodePropertyShape(node: RDFNode, ps: PropertyShape): CheckTyping = {
    logger.debug(s"Node $node - PropertyShape ${ps.showId}")
    val path = ps.path
    val attempt = Attempt(node, RefNode(ps.id), ps.message, getSeverity(ps), Some(path))
    if (ps.deactivated) for {
      t <- addEvidence(attempt, s"Property shape ${ps.showId} is deactivated")
    } yield (t,true)
    else {
      val cs      = ps.components
      val pss     = ps.propertyShapes.toList
      for {
        r1 <- runLocal(checkAllWithTyping(cs.to(LazyList), component2PropertyChecker(ps)(attempt, path)), _.addType(node, ps))
        r2 <- runLocal(checkAllWithTyping(pss.to(LazyList), checkPropertyShapePath(path)(attempt)(node)), _.addType(node, ps))
      } yield {
        val r = combineResults(r1, r2)
        logger.debug(s"Result of node $node - PropertyShape ${ps.showId}: ${showResult(r)}")
        val finalR: Result = if (r._2) {
          (r._1.addEvidence(node,ps,s"$node satisfies property shape $ps"), true)
        } else {
          (r._1.addNotEvidence(node,ps,shapesFailed(node,ps,Set(),attempt,"Property shape failed")),false)
        }
        logger.debug(s"Result of chechPropertyShape($node,${ps.showId})=${showResult(finalR)}")
        finalR

      }
    }
  }

  private def checkNodeShape(shape: Shape): NodeChecker = attempt => node => {
    logger.debug(s"checkNodeShape($node,${shape.showId})")
    if (shape.deactivated) {
      logger.debug(s"Node shape is deactivated")
      for {
        t <- addEvidence(attempt, s"NodeShape ${shape.showId} is deactivated")
      } yield (t,true)
    }
    else
      for {
      r1 <- checkComponents(shape.components.toList)(attempt)(node)
      r2 <- checkPropertyShapes(shape.propertyShapes.toList)(attempt)(node)
      r = combineResults(r1, r2)
      r1 <- if (shape.closed) for {
        predicates <- predicatesInPropertyConstraints(shape, attempt, node)
        c <- checkClosed(shape.ignoredProperties, predicates)(attempt)(node)
      } yield c
      else ok(r)
    } yield {
      logger.debug(s"Result of checkNodeShape($node,${shape.showId})=\n${r1}")
      r1
    }
  }

  private def predicatesInPropertyConstraints(shape: Shape,
                                              attempt: Attempt,
                                              node: RDFNode): Check[List[IRI]] = for {
    shapes <- getPropertyShapeRefs(shape.propertyShapes.toList, attempt, node)
  } yield shapes.map(_.predicate).collect { case Some(iri) => iri }

  private def checkPropertyShape(attempt: Attempt)
                                (node: RDFNode)
                                (ps:PropertyShape): CheckTyping = {
    nodePropertyShape(node,ps)
  }

  private def checkPropertyShapePath(path: SHACLPath)
                                    (attempt: Attempt)
                                    (node: RDFNode)
                                    (sref: RefNode): CheckTyping = {
    logger.info(s"checkPropertyShapePath $node $sref path: ${path.show}")
    for {
      ps <- getPropertyShapeRef(sref, attempt, node)
      rdf <- getRDF
      os <- fromEither(rdf.objectsWithPath(node, path).leftMap(MsgError(_)))
      // _ <- debug(s"checkPropertyShapePath: os=$os\nnode: $node, path=${path.show}")
      shape <- getShapeRef(sref,attempt,node)
      r <- checkAllWithTyping(os.to(LazyList),(o: RDFNode) => {
        val newAttempt = Attempt(o, sref, shape.message, getSeverity(shape), Some(path))
        checkPropertyShape(newAttempt)(o)(ps)
      })
    } yield r
  }

  private def checkPropertyShapes(shapeRefs: List[RefNode]): NodeChecker = attempt => node => {
    logger.debug(s"Check propertyShapes($node, ${shapeRefs.map(_.showId).mkString(",")})")
    for {
      pss <- getPropertyShapeRefs(shapeRefs, attempt, node)
      r <- checkAllWithTyping(pss.to(LazyList), checkPropertyShape(attempt)(node))
    } yield {
      logger.debug(s"Result of check propertyShapes($node, ${shapeRefs.map(_.showId).mkString(",")})=${showResult(r)}")
      r
    }
  }


  private def checkComponents(cs: List[Component]): NodeChecker = attempt => node => {
    logger.debug(s"chechComponents($node,...)")
    checkAllWithTyping(cs.to(LazyList), (c: Component) => checkComponent(c)(attempt)(node))
  }


  private def checkComponent(c: Component): NodeChecker = attempt => node => {
    logger.debug(s"chechComponent($node,${c})")
    component2Checker(c)(attempt)(node)
  }

  private def component2Checker(c: Component): NodeChecker = attempt => node => {
    logger.debug(s"component2Checker($c")
    c match {
      case NodeComponent(s) => nodeComponentChecker(s)(attempt)(node)
      case Datatype(d) => datatypeChecker(d)(attempt)(node)
      case NodeKind(k) => nodeKindChecker(k)(attempt)(node)
      case MinExclusive(n) => minExclusive(n)(attempt)(node)
      case MaxExclusive(n) => maxExclusive(n)(attempt)(node)
      case MinInclusive(n) => minInclusive(n)(attempt)(node)
      case MaxInclusive(n) => maxInclusive(n)(attempt)(node)
      case MinLength(n) => minLength(n)(attempt)(node)
      case MaxLength(n) => maxLength(n)(attempt)(node)
      case Pattern(s, flags) => pattern(s, flags)(attempt)(node)
      case LanguageIn(langs) => languageIn(langs)(attempt)(node)
      case Equals(p) => equals(p)(attempt)(node)
      case Disjoint(p) => disjoint(p)(attempt)(node)
      case LessThan(p) => lessThan(p)(attempt)(node)
      case LessThanOrEquals(p) => lessThanOrEquals(p)(attempt)(node)
      case Xone(shapes) => xone(shapes)(attempt)(node)
      case And(shapes) => and(shapes)(attempt)(node)
      case Or(shapes) => or(shapes)(attempt)(node)
      case Not(shape) => not(shape)(attempt)(node)
      case ClassComponent(cls) => classComponentChecker(cls)(attempt)(node)
      case HasValue(v) => hasValue(v)(attempt)(node)
      case In(ls) => inChecker(ls)(attempt)(node)
      case _ => unsupportedNodeChecker(s"Node constraint: $c")(attempt)(node)
    }
  }

  private def checkValues(ls: List[RDFNode], p: RDFNode => CheckTyping): CheckTyping = for {
    ts <- checkList(ls, p)
    r <- combineResultSeq(ts)
  } yield r

/*  private def propertyShape2PropertyChecker(attempt: Attempt, path: SHACLPath)
                                           (psref: RefNode): CheckTyping = {
    logger.debug(s"propertyShape2PropertyChecker. path: $path, propertyShape: $psref")
    val node = attempt.node
    for {
      ps <- getPropertyShapeRef(psref, attempt, node)
      rdf <- getRDF
      os = rdf.objectsWithPath(node, path).toList
      shape <- getShapeRef(psref,attempt,node)
      _ <- debug(s"propertyShape2PropertyChecker: $os for $path")
      check: CheckTyping = checkValues(os, o => {
        val newAttempt = Attempt(o, psref, shape.message, getSeverity(shape), Some(path))
        checkPropertyShape(newAttempt)(o)(ps)
      }
      )
      r <- check
      _ <- debug(s"Result of propertyShape2PropertyChecker\n${showResult(r)}")
    } yield r
  } */

  private def component2PropertyChecker(p: PropertyShape)(attempt: Attempt, path: SHACLPath)(c: Component): CheckTyping = {
    logger.debug(s"component2PropertyChecker. propertyShape: $p, path: $path, component: $c")
    for {
      rdf <- getRDF
      node = attempt.node
      os <- fromEither(rdf.objectsWithPath(node, path).leftMap(MsgError(_)))
      ls = os.toList
      check: CheckTyping = c match {
        case MinCount(n) => minCount(n, ls, attempt, path)
        case MaxCount(n) => maxCount(n, ls, attempt, path)
        case UniqueLang(v) => uniqueLang(v, ls, attempt, path)
        case QualifiedValueShape(shape, min, max, disjoint) =>
          qualifiedValueShape(shape, p, min, max, disjoint, ls, attempt, path)
        case HasValue(v) => {
          logger.debug(s"HasValuePropertyChecker(v = $v, ls=$ls, node = $node, path= $path")
          hasValuePropertyChecker(v, ls, attempt, node, path)
        }
        case Equals(p) => for {
          values <- fromEither(rdf.objectsWithPath(node, PredicatePath(p)).leftMap(MsgError(_)))
          v <- equalsPath(ls, values.toList, p, attempt, path)
        } yield v
        case _ => checkValues(ls, component2Checker(c)(attempt)(_))
      }
      t <- check
    } yield t
  }

  private def nodeComponentChecker(sref: RefNode): NodeChecker = attempt => node => {
    for {
      s <- getShapeRef(sref, attempt, node)
      typing <- getTyping
      // shape <- getShapeRef(sref, attempt, node)
      r <- if (typing.getOkValues(node).contains(s)) done
      else if (typing.getFailedValues(node).contains(s)) fail(s"getFailedValues($node) already contains $s")
      else runLocal(nodeShape(node, s), _.addType(node, s))
      t <- if (r._2) addEvidence(attempt, s"$node has shape ${s.id}")
      else for {
        shape <- getShapeRef(attempt.shapeRef,attempt,node)
        t1 <- addNotEvidence(attempt,errorNode(node,shape,attempt,s"$node does not have shape ${s.id}"),s"$node does not have shape ${s.id}")
      } yield t1
    } yield {
      // println(s"NodeComponentChecker: ref $sref, attempt: $attempt")
      (t,r._2)
    }
  }

  private def classComponentChecker(cls: RDFNode): NodeChecker = attempt => node => {
    for {
      rdf <- getRDF
      b <- fromEither(rdf.hasSHACLClass(node, cls).leftMap(MsgError(_)))
      t <- condition(b,attempt,
        classError(node, cls, attempt),
        s"$node is in class $cls")
    } yield t
  }

  private def nodeKindChecker(k: NodeKindType): NodeChecker = attempt => node => {
    logger.debug(s"nodeKindChecker($node,$k)")
    k match {
      case IRIKind => iriChecker(attempt)(node)
      case LiteralKind => literalChecker(attempt)(node)
      case BlankNodeKind => blankNodeChecker(attempt)(node)
      case BlankNodeOrIRI => blankNodeOrIRIChecker(attempt)(node)
      case BlankNodeOrLiteral => blankNodeOrLiteralChecker(attempt)(node)
      case IRIOrLiteral => iriOrLiteralChecker(attempt)(node)
    }
  }

  private def datatypeChecker(d: IRI): NodeChecker = attempt => node => for {
    rdf <- getRDF
    t <- condition(
      hasDatatype(rdf, node, d),
      attempt,
      datatypeError(node, d, attempt),
      s"$node has datatype $d")
  } yield t


  private def unsupportedNodeChecker(msg: String): NodeChecker = attempt => node =>
    fail(s"Unsupported feature: $msg")

  private def iriChecker: NodeChecker = attempt => node => {
    condition(node.isIRI, attempt,
      iriKindError(node, attempt),
      s"$node is an IRI", "iri")
  }

  def compare(control: RDFNode,
              comparison: (RDFNode, RDFNode) => Either[String,Boolean],
              err: (RDFNode, Attempt, RDFNode) => ValidationResult,
              msg: String
              ): NodeChecker = attempt => node => {
    val c = comparison(control, node).getOrElse(false)
    for {
      t <- condition(c, attempt,
        err(node, attempt, control),
        s"$node satisfies $msg(${control})")
    } yield t
  }

  def compareLiterals(n: Literal,
                      f: (NumericLiteral, NumericLiteral) => Boolean,
                      err: (RDFNode, Attempt, RDFNode) => ValidationResult,
                      msg: String
                     ): NodeChecker = attempt => node => for {
    ctrolValue <- checkNumeric(n, attempt)
    value <- checkNumeric(node, attempt)
    t <- condition(f(ctrolValue, value), attempt,
      err(node, attempt, n),
      s"$node satisfies $msg(${n})")
  } yield t

  private def minExclusive(n: Literal): NodeChecker =
    compare(n, lessThanNodes, minExclusiveError, "minExclusive")

  private def minInclusive(n: Literal): NodeChecker = {
    compare(n, lessThanOrEqualsNodes, minInclusiveError, "minInclusive")
  }

  private def maxExclusive(n: Literal): NodeChecker =
    compare(n, greaterThanNodes, maxExclusiveError, "maxExclusive")

  private def maxInclusive(n: Literal): NodeChecker =
    compare(n, greaterThanOrEqualsNodes, maxInclusiveError, "maxInclusive")

  private def minLength(n: Int): NodeChecker = attempt => node =>
    condition(!node.isBNode && node.getLexicalForm.length >= n, attempt,
      minLengthError(node, attempt, n),
      s"$node satisfies minLength($n)")

  private def maxLength(n: Int): NodeChecker = attempt => node =>
    condition(!node.isBNode && node.getLexicalForm.length <= n, attempt,
      maxLengthError(node, attempt, n),
      s"$node satisfies maxLength($n)")

  private def pattern(p: String, flags: Option[String]): NodeChecker = attempt => node => for {
    b <- regexMatch(p, flags, node.getLexicalForm, node, attempt)
    t <- condition(!node.isBNode && b, attempt, patternError(node, attempt, p, flags),
      s"$node satisfies pattern ~/$p/${flags.getOrElse("")}")
  } yield t

  private def regexMatch(p: String, flags: Option[String], str: String, node: RDFNode, attempt: Attempt): Check[Boolean] =
    RegEx(p, flags).matches(str) match {
      case Left(msg) => err(regexError(node, attempt, msg))
      case Right(b) => ok(b)
    }

  private def uniqueLang(b: Boolean, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): CheckTyping = if (b) {
    val node = attempt.node
    for {
      t <- condition(checkUniqueLang(os), attempt,
        uniqueLangError(node, attempt, path, os),
        s"Checked uniqueLang(true) for path $path on node $node")
    } yield t
  } else done

  private def checkUniqueLang(os: Seq[RDFNode]): Boolean = {
    def getLanguageTag(n: RDFNode): Option[String] = {
      n match {
        case LangLiteral(_, l) => Some(l.lang)
        case _ => None
      }
    }
    val langs: Seq[String] = os.map(getLanguageTag).flatten

    // If there are duplicated langs, the following condition fails
    langs.distinct.size == langs.size
  }

  private def languageIn(langs: List[String]): NodeChecker = attempt => node => for {
    t <- condition(checkLangIn(node, langs), attempt,
      languageInError(node, attempt, langs),
      s"$node satisfies languageIn(${langs.mkString(",")})")
  } yield t

  private def checkLangIn(node: RDFNode, langs: List[String]): Boolean = {
    node match {
      case LangLiteral(_, l) => langs.contains(l.lang)
      case _ => false
    }
  }

  private def lessThanOrEqualsNodes(n1: RDFNode, n2: RDFNode): Either[String,Boolean] =
    for {
      c1 <- n1.lessThan(n2)
      c2 <- n1.isEqualTo(n2)
    } yield c1 || c2

  private def lessThanNodes(n1: RDFNode, n2: RDFNode): Either[String, Boolean] =
    n1.lessThan(n2)

  private def greaterThanNodes(n1: RDFNode, n2: RDFNode): Either[String,Boolean] =
    n2.lessThan(n1)

  private def greaterThanOrEqualsNodes(n1: RDFNode, n2: RDFNode): Either[String,Boolean] =
    for {
     c1 <- n2.lessThan(n1)
     c2 <- n2.isEqualTo(n1)
    } yield c1 || c2

  def equals(p: IRI): NodeChecker =
    comparison(p, "equals", equalsError, equalsNode)
  def disjoint(p: IRI): NodeChecker =
    comparison(p, "disjoint", disjointError, disjointNode)
  def lessThan(p: IRI): NodeChecker =
    comparison(p, "lessThan", lessThanError, lessThanNode)
  def lessThanOrEquals(p: IRI): NodeChecker =
    comparison(p, "lessThanOrEquals", lessThanOrEqualsError, lessThanOrEqualNode)


  // TODO: Maybe add a check to see if the nodes are comparable
  // With current definition, if nodes are not comparable, always returns false without raising any error...
  private def comparison(
                          p: IRI,
                          name: String,
                          errorMaker: (RDFNode, Attempt, IRI, Set[RDFNode]) => ValidationResult,
                          cond: (RDFNode, RDFNode) => Boolean): NodeChecker =
    attempt => node => {
      logger.debug(s"Comparison on node $node")
      for {
        rdf <- getRDF
        subject = attempt.node
        vs <- fromEither(rdf.triplesWithSubjectPredicate(subject, p).leftMap(MsgError(_)))
        os = vs.map(_.obj)
        t <- {
          logger.debug(s"Values: $vs")
          if (os.isEmpty) for {
            t1 <- addNotEvidence(attempt,errorMaker(node,attempt,p,os),s"No values for node $subject with predicate $p")
          } yield (t1,false)
          else
          condition(os.forall(cond(node, _)), attempt,
            errorMaker(node, attempt, p, os),
            s"$node satisfies $name $p with values ${os})")
        }
      } yield t
    }

  private def and(srefs: Seq[RefNode]): NodeChecker = attempt => node => {
    for {
      shapes <- getShapeRefs(srefs.toList, attempt, node)
      r <- checkAllWithTyping(shapes.to(LazyList), (s: Shape) => nodeShape(node,s))
    } yield r
  }

/*  private def checkAnd(node: RDFNode, shapes: List[Shape], t: ShapeTyping) : Boolean = {
    t.getFailedValues(node).isEmpty
  } */

  private def xone(sRefs: Seq[RefNode]): NodeChecker = attempt => node => {
    for {
      t <- getTyping
      // shapes <- getShapeRefs(sRefs.toList, attempt, node)
      r <- checkSomeFlagCount(sRefs.to(LazyList), (s: RefNode) => nodeShapeRef(node,s,attempt),t)
      count = r._2
      t1 <- condition(count == 1,
        attempt,
        xoneError(node, attempt, sRefs),
        s"$node satisfies exactly one of $sRefs")
    } yield t1
  }

  def checkXoneType(node: RDFNode, shapes: List[Shape], t: ShapeTyping): Boolean = {
    shapes.map(t.hasType(node,_)).count(_ == true) == 1
  }

  private def qualifiedValueShape(
    shape: RefNode,
    p: PropertyShape,
    min: Option[Int],
    max: Option[Int],
    maybeDisjoint: Option[Boolean],
    values: Seq[RDFNode],
    attempt: Attempt,
    path: SHACLPath): CheckTyping = {
    val disjoint = maybeDisjoint.getOrElse(false)
    for {
      t <- getTyping
      vs <- if (disjoint) filterConformSiblings(values, p, attempt)
            else ok(values)
      // cs: List[Check[ShapeTyping]] = vs.toList.map(o => nodeShapeRef(o, shape, attempt))
      r <- checkSomeFlagCount(vs.to(LazyList), (n: RDFNode) => nodeShapeRef(n, shape, attempt), t)
      value = r._2
      t <- {
        condition(between(value, min, max), attempt,
          qualifiedShapeError(attempt.node, attempt, value, min, max),
          s"qualifiedValueShape value = ${value}, min=${min.map(_.toString).getOrElse("-")}, max=${max.map(_.toString).getOrElse("-")}")
      }
    } yield {
      logger.debug(s"qualifiedValueShape(attempt: ${attempt},${shape.showId}): t=\n${showResult(t)}")
      t
    }
  }

  private def filterConformSiblings(values: Seq[RDFNode], p: PropertyShape, attempt: Attempt): Check[Seq[RDFNode]] = {
    val shapes = schema.siblingQualifiedShapes(RefNode(p.id))
    filterConformShapes(values, shapes, attempt)
  }

  private def filterConformShapes(values: Seq[RDFNode], shapes: Seq[RefNode], attempt: Attempt): Check[Seq[RDFNode]] = {
    logger.debug(s"FilterConformShapes(values=$values, shapes=$shapes)")
    def checkValuesShapes: Check[List[(RDFNode, Boolean)]] = {
      sequence(values.toList.map(value => conformsNodeShapes(value, shapes, attempt)))
    }
    for {
      cs <- checkValuesShapes
      rs = cs.collect { case (n, false) => n }
    } yield {
      logger.debug(s"Result of FilterConformShapes($values,$shapes,$attempt) = $rs")
      rs.toSeq
    }
  }

  private def conformsNodeShapes(node: RDFNode,
                         shapes: Seq[RefNode],
                         attempt: Attempt): Check[(RDFNode, Boolean)] = for {
    ls <- checkLs(shapes.toList.map(nodeShapeRef(node, _, attempt)))
  } yield (node, !ls.isEmpty)

  def between(v: Int, maybeMin: Option[Int], maybeMax: Option[Int]): Boolean = (maybeMin, maybeMax) match {
    case (None, None) => true
    case (Some(min), None) => v >= min
    case (None, Some(max)) => v <= max
    case (Some(min), Some(max)) => v >= min && v <= max
  }

  private def or(sRefs: Seq[RefNode]): NodeChecker = attempt => node => {
    val last: CheckTyping = fail(s"None of the components of or pass")
    def fn(sref: RefNode): CheckTyping = nodeShapeRef(node, sref, attempt)
    checkSomeFlag(sRefs.to(LazyList),fn,last)
  }

  private def not(sref: RefNode): NodeChecker = attempt => node => {
    for {
      shape <- getShapeRef(sref, attempt, node)
      typing <- getTyping
      t <- {
        logger.debug(s"\nTesting not nodeShape($node,${shape.showId}) with typing\n${typing}")
        nodeShape(node,shape)
      }
      t1 <- {
        logger.debug(s"\nnot($sref). Value of nodeShape($node,${shape.showId})=\n$t")
        condition(!t._1.hasType(node,shape), attempt, notShapeError(node,sref,attempt), s"$node does not have shape $sref")
      }
    } yield t1
  }

  private def checkNumeric(node: RDFNode, attempt: Attempt): Check[NumericLiteral] =
    numericValue(node).fold(e => err(notNumeric(node,attempt)),
      value => ok(value)
    )

  private def literalChecker: NodeChecker = attempt => node => {
    condition(node.isLiteral, attempt,
      literalKindError(node, attempt),
      s"$node is a Literal")
  }

  private def blankNodeChecker: NodeChecker = nodeShape => node => {
    condition(node.isBNode, nodeShape,
      bNodeKindError(node, nodeShape),
      s"$node is a Blank Node")
  }

  private def blankNodeOrIRIChecker: NodeChecker = nodeShape => node => {
    condition(node.isBNode || node.isIRI, nodeShape,
      bNodeOrIRIKindError(node, nodeShape),
      s"$node is a Blank Node or an IRI")
  }

  private def blankNodeOrLiteralChecker: NodeChecker = attempt => node => {
    condition(node.isBNode || node.isLiteral, attempt,
      bNodeOrLiteralKindError(node, attempt),
      s"$node is a Blank Node or Literal")
  }

  private def iriOrLiteralChecker: NodeChecker = attempt => node => {
    condition(node.isIRI || node.isLiteral, attempt,
      iriOrLiteralKindError(node, attempt),
      s"$node is a IRI or Literal")
  }

  private def hasValuePropertyChecker(v: Value, os: List[RDFNode], attempt: Attempt, node: RDFNode, path: SHACLPath): CheckTyping = for {
    t <- getTyping
    newT <- os.size match {
      case 0 => for {
        t1 <- addNotEvidence(attempt, hasValueErrorNoValue(node, attempt, v, path), s"HasValue($v) failed. $node has not value")
      } yield (t1,false)
      case 1 => hasValue(v)(attempt)(os.head)
      case n => for {
        t1 <- addNotEvidence(attempt, hasValueErrorMoreThanOne(node, attempt, v, path, n), s"HasValue($v) failed. $node has more $n values")
      } yield (t1,false)
    }
  } yield newT

  private def hasValue(value: Value): NodeChecker = attempt => currentNode => {
    condition(isValue(currentNode, value), attempt,
      hasValueError(currentNode, attempt, value),
      s"Checked $currentNode sh:hasValue $value")
  }

  private def inChecker(values: Seq[Value]): NodeChecker = attempt => currentNode => {
    condition(inValues(currentNode, values), attempt,
      inError(currentNode, attempt, values),
      s"Checked $currentNode sh:in $values")
  }

  private def minCount(minCount: Int, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): CheckTyping = {
    logger.debug(s"minCount $minCount, os: $os, attempt: $attempt, path: $path")
    val count = os.size
    val node = attempt.node
    condition(count >= minCount, attempt,
      minCountError(node, attempt, minCount, os.size),
      s"Checked minCount($minCount) for path($path) on node $node")
  }

  private def maxCount(maxCount: Int, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): CheckTyping = {
    val count = os.size
    val node = attempt.node
    condition(count <= maxCount, attempt,
      maxCountError(node, attempt, maxCount, count),
      s"Checked maxCount($maxCount) for path($path) on node $node")
  }

  private def equalsPath(os: List[RDFNode], values: List[RDFNode], equalsIri: IRI, attempt: Attempt, path: SHACLPath): CheckTyping = {
    logger.debug(s"equalsPath $equalsIri, os: $os, values: $values, attempt: $attempt, path: $path")
    Comparisons.different(os.toList,values.toList) match {
      case Left(msg) => for {
        t <- addNotEvidence(attempt, equalsError(attempt.node,attempt,equalsIri,Set()),s"node ${attempt.node} fails equals condition. Error: $msg")
      } yield (t,false)
      case Right(List()) => for {
       t <- addEvidence(attempt,
         s"equals(${equalsIri.show}. nodes ${os.show} pass equals condition with values ${values.show}")
      } yield (t,true)

      case Right(ls) => checkAllWithTyping(ls.to(LazyList), (n: RDFNode) => for {
       t <- addNotEvidence(attempt, equalsError(n,attempt,equalsIri,Set()),s"node $n fails equals condition. ")
      } yield (t, false))
    }
  }

  private def checkClosed(ignoredProperties: List[IRI], allowedProperties: List[IRI]): NodeChecker = attempt => node => {
    logger.debug(s"checkClosed(ignored=$ignoredProperties, allowed=$allowedProperties")
    for {
      rdf <- getRDF
      neighbours <- fromEither(rdf.triplesWithSubject(node).leftMap(MsgError(_)))
      predicates = neighbours.map(_.pred).toList
      notAllowed = predicates.diff(ignoredProperties).diff(allowedProperties)
      t <- {
        condition(notAllowed.isEmpty, attempt,
          closedError(node, attempt, allowedProperties, ignoredProperties, notAllowed),
          s"Passes closed condition with predicates $predicates and ignoredProperties $ignoredProperties")
      }
    } yield t
  }

  private def getShapeRefs(sRefs: List[RefNode], attempt: Attempt, node: RDFNode): Check[List[Shape]] =
    sequence(sRefs.map(getShapeRef(_, attempt, node)))

  private def getPropertyShapeRefs(srefs: List[RefNode], attempt: Attempt, node: RDFNode): Check[List[PropertyShape]] =
    sequence(srefs.map(getPropertyShapeRef(_, attempt, node)))

  private def getPropertyShapeRef(sref: RefNode, attempt: Attempt, node: RDFNode): Check[PropertyShape] = for {
    shape <- getShapeRef(sref, attempt, node)
    ps <- shape2PropertyShape(shape, attempt, node)
  } yield ps

  private def shape2PropertyShape(shape: Shape, attempt: Attempt, node: RDFNode): Check[PropertyShape] = shape match {
    case ps: PropertyShape => ok(ps)
    case _ => err(expectedPropertyShape(node, attempt, s"Expected shape $shape to be a property shape"))
  }

  private def addEvidence(attempt: Attempt, msg: String): Check[ShapeTyping] = {
    for {
      t <- getTyping
      shape <- getShapeRef(attempt.shapeRef, attempt, attempt.node)
      _ <- addLog(List(NodeShapeEvidence(attempt.node, attempt.shapeRef, msg)))
    } yield t.addEvidence(attempt.node, shape, msg)
  }

  private def addNotEvidence(attempt: Attempt,
                             e: AbstractResult,
                             msg: String
                            ): Check[ShapeTyping] = {
    val node = attempt.node
    val sref = attempt.shapeRef
    for {
      t <- getTyping
      shape <- getShapeRef(sref, attempt, node)
      _ <- addLog(List(NodeShapeEvidence(attempt.node, sref, msg)))
    } yield {
      t.addNotEvidence(node, shape, e)
    }
  }

  private def getShapeRef(sref: RefNode, attempt: Attempt, node: RDFNode): Check[Shape] =
    schema.shapesMap.get(sref) match {
      case Some(shape) => ok(shape)
      case None => err(notFoundShapeRef(node, attempt,
        s"Shape ${sref.showId} not found in schema. Available srefs: ${schema.shapesMap.keys.map(_.showId).mkString(",")}"))
    }

  def validateAll(rdf: RDFReader): CheckResult[AbstractResult, (ShapeTyping, Boolean), Log] = {
    runCheck(checkSchemaAll, rdf)
  }

  def showResult(t: (ShapeTyping, Boolean)): String =
    t.show

  ////////////////////////////////////////////


  /**
    * if condition is true adds an evidence, otherwise, adds a not typing with the Violation error as evidence
    * @param condition condition to check
    * @param attempt current validation attempt that is being tried
    * @param error error to raise in case `condition` is false
    * @param evidence evidence to add to `attempt` in case `condition` is true
    */
  private[validator] def condition(condition: Boolean,
                                   attempt: Attempt,
                                   error: AbstractResult,
                                   evidence: String,
                                   conditionName: String = ""): CheckTyping = {
    logger.debug(s"condition($conditionName,...)")
    for {
      t <- getTyping
      r <- condFlag(validateCheck(condition, error),
        (_: Unit) => addEvidence(attempt, evidence),
        err => addNotEvidence(attempt, err, "Condition failed")
      )
    } yield {
      logger.debug(s"result of condition: $r")
      r
    }
  }

  // TODO: Refactor the following code...
  // move to SRDF and check SPARQL compatibility
  // SPARQL comparison opetators: https://www.w3.org/TR/sparql11-query/#OperatorMapping
  private def equalsNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (l1: Literal, l2: Literal) => l1 == l2
    case (i1: IRI, i2: IRI) => i1 == i2
    case (b1: BNode, b2: BNode) => b1 == b2
    case (_, _) => false
  }

  private def disjointNode(n1: RDFNode, n2: RDFNode): Boolean = n1 != n2
  private def lessThanNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (IntegerLiteral(n1,_), IntegerLiteral(n2,_)) => n1 < n2
    case (DecimalLiteral(n1,_), DecimalLiteral(n2,_)) => n1 < n2
    case (DoubleLiteral(n1,_), DoubleLiteral(n2,_)) => n1 < n2
    case (StringLiteral(n1), StringLiteral(n2)) => n1 < n2
    case (DatatypeLiteral(n1, d1), DatatypeLiteral(n2, d2)) => d1 == d2 && n1 < n2
    case (LangLiteral(n1, l1), LangLiteral(n2, l2)) => n1 < n2
    case (i1: IRI, i2: IRI) => i1.str < i2.str
    case (b1: BNode, b2: BNode) => b1.id < b2.id
    case (_, _) => false
  }
  private def lessThanOrEqualNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (IntegerLiteral(n1,_), IntegerLiteral(n2,_)) => n1 <= n2
    case (DecimalLiteral(n1,_), DecimalLiteral(n2,_)) => n1 <= n2
    case (DoubleLiteral(n1,_), DoubleLiteral(n2,_)) => n1 <= n2
    case (StringLiteral(n1), StringLiteral(n2)) => n1 <= n2
    case (DatatypeLiteral(n1, d1), DatatypeLiteral(n2, d2)) => d1 == d2 && n1 <= n2
    case (LangLiteral(n1, l1), LangLiteral(n2, l2)) => n1 <= n2
    case (i1: IRI, i2: IRI) => i1.str <= i2.str
    case (b1: BNode, b2: BNode) => b1.id <= b2.id
    case (_, _) => false
  }
  /**
    * Checks that `node` is one of `values`
    */
  private def inValues(node: RDFNode, values: Seq[Value]): Boolean = {
    values.exists(_.matchNode(node))
  }

  private def isValue(node: RDFNode, value: Value): Boolean = {
    value.matchNode(node)
  }

  private def hasDatatype(rdf: RDFReader, node: RDFNode, d: IRI): Boolean = {
    rdf.checkDatatype(node, d) match {
      case Left(msg) => false
      case Right(true) => true
      case Right(false) => false
    }
  }

  private[validator] def debug(msg: String): Check[Unit] = {
    logger.debug(msg)
    ok(())
  }

}

object Validator {
  def empty = Validator(schema = Schema.empty)

  def validate(schema: Schema, rdf: RDFReader): Either[AbstractResult, (ShapeTyping, Boolean)] = {
    Validator(schema).validateAll(rdf).result
  }

}

