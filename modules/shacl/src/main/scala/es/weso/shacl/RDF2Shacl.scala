package es.weso.shacl
import scala.util.{Failure, Success, Try}
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.PREFIXES._
import es.weso.utils.TryUtils._
import cats._
import cats.implicits._
import SHACLPrefixes._
import com.typesafe.scalalogging.LazyLogging

object RDF2Shacl extends RDFParser with LazyLogging {

  // TODO: Move this declaration to RDFParser
  implicit val applicativeRDFParser = new Applicative[RDFParser] {
    def pure[A](x: A) = (n,rdf) => Success(x)

    def ap[A,B](ff:RDFParser[A => B])(fa:RDFParser[A]): RDFParser[B] = (n,f) => {
      fa(n,f) match {
        case Success(a) => ff(n,f) match {
          case Success(f) => Success(f(a))
          case Failure(e) => Failure(e)
        }
        case Failure(e) => Failure(e)
      }
    }
  }
  // Keep track of parsed shapes
  val parsedShapes = collection.mutable.Map[RDFNode,NodeShape]()

  /**
   * Parses RDF content and obtains a SHACL Schema and a PrefixMap
   */
  def getShacl(rdf: RDFReader): Try[Schema] = {
    parsedShapes.clear()
    val pm = rdf.getPrefixMap
    for {
      shapes <- shapes(rdf)
    } yield (Schema(pm,shapes))
  }

  def shapes(rdf: RDFReader): Try[Seq[NodeShape]] = {
   val shape_nodes = subjectsWithType(sh_NodeShape, rdf)
   filterSuccess(shape_nodes.toSeq.map (node => shape(node,rdf)))
  }

  def shape(node: RDFNode, rdf: RDFReader): Try[NodeShape] = {
    if (parsedShapes.contains(node)) Success(parsedShapes(node))
    else {
      val maybeId = node match {
        case iri: IRI => Some(iri)
        case _ => None
      }
      val shape = NodeShape.empty
      parsedShapes += (node -> shape)
      for {
        targets <- targets(node, rdf)
        filters <- filters(node, rdf)
        constraints <- constraints(node, rdf)
        closed <- booleanFromPredicateOptional(sh_closed)(node, rdf)
        ignoredNodes <- rdfListForPredicateOptional(sh_ignoredProperties)(node, rdf)
        ignoredIRIs <- fromEitherString(nodes2iris(ignoredNodes))
      } yield {
        val newShape = shape.copy(
          id = maybeId,
          targets = targets,
          constraints = constraints,
          closed = closed.getOrElse(false),
          ignoredProperties = ignoredIRIs
        )
        parsedShapes(node) = newShape
        newShape
      }
    }
  }

  def targets: RDFParser[Seq[Target]] =
    combineAll(
          targetNodes,
          targetClasses,
          implicitTargetClass,
          targetSubjectsOf,
          targetObjectsOf
        )

  def targetNodes: RDFParser[Seq[Target]] = (n,rdf) => {
    val attempts = for {
      ns <- objectsFromPredicate(sh_targetNode)(n,rdf)
    } yield {
      val xs = ns.toSeq.map(mkTargetNode)
      filterSuccess(xs)
    }
    attempts.flatten
  }

  def targetClasses: RDFParser[Seq[Target]] = (n,rdf) => {
    val attempts = for {
      ns <- objectsFromPredicate(sh_targetClass)(n,rdf)
    } yield {
      val xs = ns.toSeq.map(mkTargetClass)
      filterSuccess(xs)
    }
    attempts.flatten
  }

  def implicitTargetClass: RDFParser[Seq[Target]] = (n,rdf) => {
    val shapeTypes = rdf.triplesWithSubjectPredicate(n, rdf_type).map(_.obj)
    val rdfs_Class = rdfs + "Class"
    if (shapeTypes.contains(rdfs_Class))
      mkTargetClass(n).map(Seq(_))
    else
      Success(Seq())
  }

  def targetSubjectsOf: RDFParser[Seq[Target]] = (n,rdf) => {
    val attempts = for {
      ns <- objectsFromPredicate(sh_targetSubjectsOf)(n,rdf)
    } yield {
      val xs = ns.toSeq.map(mkTargetSubjectsOf)
      filterSuccess(xs)
    }
    attempts.flatten
  }

  def targetObjectsOf: RDFParser[Seq[Target]] = (n,rdf) => {
    val attempts = for {
      ns <- objectsFromPredicate(sh_targetObjectsOf)(n,rdf)
    } yield {
      val xs = ns.toSeq.map(mkTargetObjectsOf)
      filterSuccess(xs)
    }
    attempts.flatten
  }

  def mkTargetNode(n: RDFNode): Try[TargetNode] = Success(TargetNode(n))
  def mkTargetClass(n: RDFNode): Try[TargetClass] = Success(TargetClass(n))
  def mkTargetSubjectsOf(n: RDFNode): Try[TargetSubjectsOf] = n match {
    case i:IRI => Success(TargetSubjectsOf(i))
    case _ => parseFail(s"targetSubjectsOf requires an IRI. Obtained $n")
  }
  def mkTargetObjectsOf(n: RDFNode): Try[TargetObjectsOf] = n match {
    case i:IRI => Success(TargetObjectsOf(i))
    case _ => parseFail(s"targetObjectsOf requires an IRI. Obtained $n")
  }

  def filters: RDFParser[Seq[NodeShape]] = (n, rdf) => {
    // Todo add support for sh:filter
    Success(Seq())
  }

  def constraints: RDFParser[Seq[Shape]] = {
    combineAll(propertyConstraints, nodeConstraints)
  }

  def nodeConstraints: RDFParser[Seq[NodeConstraint]] = (n,rdf) => {
   for {
     cs <- components(n,rdf)
   } yield cs.map(c => NodeConstraint(components = List(c)))
  }

  def propertyConstraints: RDFParser[Seq[Shape]] = (n, rdf) => {
    val attempts = for {
      ps <- objectsFromPredicate(sh_property)(n,rdf)
    } yield {
      val xs = ps.toSeq.map(p => propertyConstraint(p,rdf))
      filterSuccess(xs)
    }
    attempts.flatten
  }

  def propertyConstraint: RDFParser[PropertyShape] = (n, rdf) => {
    val id = if (n.isIRI) Some(n.toIRI) else None
    for {
      predicate <- iriFromPredicate(sh_path)(n,rdf)
      components <- components(n,rdf)
    } yield {
      PropertyShape(id, PredicatePath(predicate), components)
    }
  }

  def components: RDFParser[Seq[Component]] =
    anyOf(
        classComponent,
        datatype,
        nodeKind,
        minCount, maxCount,
        minExclusive, maxExclusive, minInclusive, maxInclusive,
        minLength, maxLength,
        pattern,
        equals, disjoint, lessThan, lessThanOrEquals,
        or, and, not,
        shapeComponent,
        hasValue,
        in
    )


  def classComponent = parsePredicate(sh_class, ClassComponent)
  def datatype = parsePredicateIRI(sh_datatype, Datatype)
  def minInclusive = parsePredicateLiteral(sh_minInclusive, MinInclusive)
  def maxInclusive = parsePredicateLiteral(sh_maxInclusive, MaxInclusive)
  def minExclusive = parsePredicateLiteral(sh_minExclusive, MinExclusive)
  def maxExclusive = parsePredicateLiteral(sh_maxExclusive, MaxExclusive)
  def minLength = parsePredicateInt(sh_minLength, MinLength)
  def maxLength = parsePredicateInt(sh_maxLength, MaxLength)

  def pattern : RDFParser[Pattern] = (n,rdf) => for {
    pat <- stringFromPredicate(sh_pattern)(n,rdf)
    flags <- stringFromPredicateOptional(sh_flags)(n,rdf)
  } yield Pattern(pat,flags)

  def equals = parsePredicateComparison(sh_equals,Equals)
  def disjoint = parsePredicateComparison(sh_disjoint,Disjoint)
  def lessThan = parsePredicateComparison(sh_lessThan,LessThan)
  def lessThanOrEquals = parsePredicateComparison(sh_lessThanOrEquals,LessThanOrEquals)

  def parsePredicateComparison(pred: IRI, mkComp: IRI => Component): RDFParser[Component] = (n,rdf) => for {
    p <- iriFromPredicate(pred)(n,rdf)
  } yield mkComp(p)

  def or : RDFParser[Or] = (n,rdf) => for {
    shapeNodes <- rdfListForPredicate(sh_or)(n,rdf)
    if (!shapeNodes.isEmpty)
    shapes <- mapRDFParser(shapeNodes.toList,getShape)(n,rdf)
  } yield Or(shapes)

  def and : RDFParser[And] = (n,rdf) => for {
    nodes <- rdfListForPredicate(sh_and)(n,rdf)
    if (!nodes.isEmpty)
    shapes <- mapRDFParser(nodes,getShape)(n,rdf)
  } yield And(shapes)

  def not: RDFParser[Not] = (n,rdf) => for {
    shapeNode <- objectFromPredicateOptional(sh_not)(n,rdf)
    if (shapeNode.isDefined)
    shape <- getShape(shapeNode.get)(n,rdf)
  } yield Not(shape)

  def mapRDFParser[A,B](ls: List[A], p: A => RDFParser[B]): RDFParser[List[B]] = {
    ls.map(v => p(v)).sequence
  }

  def shapeComponent: RDFParser[ShapeComponent] = (n,rdf) => {
    for {
     nodeShape <- objectFromPredicate(sh_shape)(n,rdf)
     s <- getShape(nodeShape)(n,rdf)
    } yield {
      ShapeComponent(s)
    }
  }

  def getShape(node: RDFNode): RDFParser[NodeShape] = (n, rdf) =>
    if (parsedShapes.contains(node)) Success(parsedShapes(node))
    else shape(node,rdf)

  def minCount = parsePredicateInt(sh_minCount, MinCount)
  def maxCount = parsePredicateInt(sh_maxCount, MaxCount)

  def hasValue: RDFParser[Component] = (n,rdf) => {
    for {
     o <- objectFromPredicate(sh_hasValue)(n,rdf)
     v <- node2Value(o)
    } yield HasValue(v)
  }

  def in: RDFParser[Component] = (n,rdf) => {
    for {
     ns <- rdfListForPredicate(sh_in)(n,rdf)
     vs <- convert2Values(ns.map(node2Value(_)))
    } yield In(vs)
  }

  def node2Value(n: RDFNode): Try[Value] = {
    n match {
      case i: IRI => Success(IRIValue(i))
      case l: Literal => Success(LiteralValue(l))
      case _ => parseFail(s"Element $n must be a IRI or a Literal to be part of sh:in")
    }
  }

  def convert2Values[A](cs: List[Try[A]]): Try[List[A]] = {
    if (cs.isEmpty)
      parseFail("The list of values associated with sh:in must not be empty")
    else {
      filterSuccess(cs).map(_.toList)
    }
  }

  def nodeKind: RDFParser[Component] = (n,rdf) => {
    logger.debug(s"Searching for nodeKind on ${n}")
    for {
      os <- objectsFromPredicate(sh_nodeKind)(n,rdf)
      nk <- {
        logger.debug(s"Objects with nodeKind: $os")
        parseNodeKind(os)
      }
    } yield {
      logger.debug(s"Nodekind found: $nk")
      nk
    }
  }

  def parseNodeKind(os: Set[RDFNode]): Try[Component] = {
    logger.debug(s"ParseNodeKind($os)")
    os.size match {
      case 0 => parseFail("no objects of nodeKind property")
      case 1 => {
        os.head match {
          case nk: IRI => nk match {
            case `sh_IRI` => Success(NodeKind(IRIKind))
            case `sh_BlankNode` => Success(NodeKind(BlankNodeKind))
            case `sh_Literal` => Success(NodeKind(LiteralKind))
            case `sh_BlankNodeOrLiteral` => Success(NodeKind(BlankNodeOrLiteral))
            case `sh_BlankNodeOrIRI` => Success(NodeKind(BlankNodeOrIRI))
            case `sh_IRIOrLiteral` => Success(NodeKind(IRIOrLiteral))
            case x => {
              logger.error(s"incorrect value of nodeKind property $x")
              parseFail(s"incorrect value of nodeKind property $x")
            }
          }
          case x => {
            logger.error(s"incorrect value of nodeKind property $x")
            parseFail(s"incorrect value of nodeKind property $x")
        }
       }
      }
      case n => parseFail(s"objects of nodeKind property > 1. $os")
    }
  }


  def parsePredicateLiteral[A](p: IRI, maker: Literal => A): RDFParser[A] = (n,rdf) => for {
    v <- literalFromPredicate(p)(n,rdf)
  } yield maker(v)

  def parsePredicateInt[A](p: IRI, maker: Int => A): RDFParser[A] = (n,rdf) => for {
    v <- integerLiteralForPredicate(p)(n,rdf)
  } yield maker(v.intValue())

  def parsePredicateString[A](p: IRI, maker: String => A): RDFParser[A] = (n,rdf) => for {
    v <- stringFromPredicate(p)(n,rdf)
  } yield maker(v)

  def parsePredicate[A](p: IRI, maker: RDFNode => A): RDFParser[A] = (n,rdf) => for {
    o <- objectFromPredicate(p)(n,rdf)
  } yield maker(o)

  def parsePredicateIRI[A](p: IRI, maker: IRI => A): RDFParser[A] = (n,rdf) => for {
    iri <- iriFromPredicate(p)(n,rdf)
  } yield maker(iri)

/////////////////////////////////////////////////////////


  // TODO: Move these methods to SRDF project

  /**
   * Combine a sequence of RDFParsers
   *
   */
  def combineAll[A](ps: RDFParser[Seq[A]]*): RDFParser[Seq[A]] = {
    val zero : RDFParser[Seq[A]] = (_,_) => Success(Seq())
    ps.foldLeft(zero)(combine)
  }

  def combine[A](p1: RDFParser[Seq[A]], p2: RDFParser[Seq[A]]): RDFParser[Seq[A]] = (n,rdf) => {
    for {
      vs1 <- p1(n,rdf)
      vs2 <- p2(n,rdf)
    } yield {
      vs1 ++ vs2
    }
  }

  def rdfListForPredicateOptional(p: IRI): RDFParser[List[RDFNode]] = for {
    maybeLs <- optional(rdfListForPredicate(p))
  } yield maybeLs.fold(List[RDFNode]())(ls => ls)

  def literalFromPredicate(p: IRI): RDFParser[Literal] = (n,rdf) => for {
    o <- objectFromPredicate(p)(n,rdf)
    r <- o match {
      case l: Literal => Success(l)
      case _ => parseFail("Value of predicate must be a literal")
    }
  } yield r

  def booleanFromPredicateOptional(p: IRI): RDFParser[Option[Boolean]] = (n,rdf) => {
    objectFromPredicateOptional(p)(n,rdf) match {
      case Success(None) => Success(None)
      case Success(Some(BooleanLiteral(b))) => Success(Some(b))
      case Success(Some(o)) => parseFail(s"value of $p must be a boolean literal. Obtained $o")
      case Failure(e) => Failure(e)
    }
  }

  def irisFromPredicate(p: IRI): RDFParser[List[IRI]] = (n,rdf) => {
    val r = objectsFromPredicate(p)(n,rdf)
    r match {
      case Success(ns) => {
        nodes2iris(ns.toList) match {
          case Right(iris) => Success(iris)
          case Left(msg) => parseFail(msg)
        }
      }
      case Failure(f) => Failure(f)
    }
  }

  def nodes2iris(ns: List[RDFNode]): Either[String, List[IRI]] = {
    sequenceEither(ns.map(node2IRI(_)))
  }

  // Todo: Use "sequence" when I find why it gives a type error...
  def sequenceEither[E,A](xs: List[Either[E,A]]): Either[E,List[A]] = {
    val zero: Either[E,List[A]] = Either.right(List())
    def next(r: Either[E,List[A]], x: Either[E,A]): Either[E,List[A]] =
      x match {
        case Left(e) => Left(e)
        case Right(v) => r match {
          case Left(e) => Left(e)
          case Right(vs) => Right(v :: vs)
        }
      }
    xs.foldLeft(zero)(next)
  }

  def node2IRI(node: RDFNode): Either[String,IRI] = node match {
    case (i: IRI) => Right(i)
    case _ => Left(s"$node is not an IRI\n")
  }

  def fromEitherString[A](e: Either[String,A]): Try[A] =
    e.fold(str => parseFail(str),v => Success(v))

  def noTarget: Seq[Target] = Seq()
  def noFilters: Seq[NodeShape] = Seq()
  def noConstraints: Seq[Shape] = Seq()

}
