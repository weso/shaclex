package es.weso.shacl.converter

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.path._
import es.weso.shacl.SHACLPrefixes._
import es.weso.shacl._
import es.weso.utils.TryUtils._

import scala.util.{Failure, Success, Try}

object RDF2Shacl extends RDFParser with LazyLogging {

  // Keep track of parsed shapes
  // TODO: Refactor this code to use a StateT
  val parsedShapes = collection.mutable.Map[ShapeRef,Shape]()

  /**
   * Parses RDF content and obtains a SHACL Schema and a PrefixMap
   */
  def getShacl(rdf: RDFReader): Try[Schema] = {
    parsedShapes.clear()
    val pm = rdf.getPrefixMap
    for {
      shapesMap <- shapesMap(rdf)
    } yield (Schema(pm,shapesMap))
  }

  def shapesMap(rdf: RDFReader): Try[Map[ShapeRef,Shape]] = {
   val nodeShapes = subjectsWithType(sh_NodeShape, rdf)
   val propertyShapes = subjectsWithType(sh_PropertyShape, rdf)
   val shapes = subjectsWithType(sh_Shape, rdf)
   val allShapes: Set[RDFNode] = nodeShapes ++ propertyShapes ++ shapes
   for {
     _ <- filterSuccess(allShapes.toSeq.map(shape(_,rdf)))
   } yield parsedShapes.toMap
  }

  def shape: RDFParser[ShapeRef] = (n,rdf) => {
    val shapeRef = ShapeRef(n)
    if (parsedShapes.contains(shapeRef)) {
      Success(shapeRef)
    }
    else {
      for {
        shapeRef <- firstOf(nodeShape, propertyShape)(n,rdf)
      } yield {
//        parsedShapes(shapeRef) = newShape
        shapeRef
      }
    }
  }

  def mkId(n: RDFNode): Option[IRI] = n match {
    case iri: IRI => Some(iri)
    case _ => None
  }

  def nodeShape: RDFParser[ShapeRef] = (n,rdf) => for {
    types <- rdfTypes(n,rdf)
    _ <- failIf(types.contains(sh_PropertyShape), "Node shapes must not have rdf:type sh:PropertyShape")(n,rdf)
    targets <- targets(n, rdf)
    propertyShapes <- propertyShapes(n, rdf)
    components <- components(n,rdf)
    closed <- booleanFromPredicateOptional(sh_closed)(n, rdf)
    ignoredNodes <- rdfListForPredicateOptional(sh_ignoredProperties)(n, rdf)
    ignoredIRIs <- fromEitherString(nodes2iris(ignoredNodes))
  } yield {
   val shape: Shape = NodeShape(id = n,
      components = components.toList,
      targets = targets,
      propertyShapes = propertyShapes,
      closed = closed.getOrElse(false),
      ignoredProperties = ignoredIRIs
    )
    val sref = ShapeRef(n)
    parsedShapes += (sref -> shape)
    sref
  }

  def propertyShape: RDFParser[ShapeRef] = (n,rdf) => for {
    types <- rdfTypes(n,rdf)
    _ <- failIf(types.contains(sh_NodeShape), "Property shapes must not have rdf:type sh:NodeShape")(n,rdf)
    targets <- targets(n, rdf)
    nodePath <- objectFromPredicate(sh_path)(n,rdf)
    path <- parsePath(nodePath,rdf)
    propertyShapes <- propertyShapes(n, rdf)
    components <- components(n,rdf)
    closed <- booleanFromPredicateOptional(sh_closed)(n, rdf)
    ignoredNodes <- rdfListForPredicateOptional(sh_ignoredProperties)(n, rdf)
    ignoredIRIs <- fromEitherString(nodes2iris(ignoredNodes))
  } yield {
    val ps = PropertyShape(id = n,
      path = path,
      components = components.toList,
      targets = targets,
      propertyShapes = propertyShapes,
      closed = closed.getOrElse(false),
      ignoredProperties = ignoredIRIs
    )
    val sref = ShapeRef(n)
    parsedShapes += (sref -> ps)
    sref
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

/*  def propertyShapes: RDFParser[Seq[PropertyShape]] = (n,rdf) =>
    if (!isPropertyShape(n,rdf)) {
      ??? // combineAll(propertyConstraints, nodeConstraints)(n,rdf)
  } else for {
    p <- propertyShape(n,rdf)
  } yield Seq(p) */

  def isPropertyShape(node: RDFNode, rdf: RDFReader): Boolean = {
    rdf.getTypes(node).contains(sh_PropertyShape) ||
    !rdf.triplesWithSubjectPredicate(node, sh_path).isEmpty
  }

/*  def nodeShapes: RDFParser[Seq[NodeShape]] = (n, rdf) => {
   val id = if (n.isIRI) Some(n.toIRI) else None
   for {
     cs <- components(n,rdf)
   } yield cs.map(c => NodeShape(id, components = List(c)))
  } */

  def propertyShapes: RDFParser[Seq[ShapeRef]] = (n, rdf) => {
    val attempts = for {
      ps <- objectsFromPredicate(sh_property)(n,rdf)
    } yield {
      val xs = ps.toSeq.map(p => propertyShape(p,rdf))
      filterSuccess(xs)
    }
    attempts.flatten
  }

  /*
  def propertyShape: RDFParser[PropertyShape] = (n, rdf) => {
    val id = if (n.isIRI) Some(n.toIRI) else None
    for {
      nodePath <- objectFromPredicate(sh_path)(n,rdf)
      path <- parsePath(nodePath,rdf)
      components <- components(n,rdf)
    } yield {
      PropertyShape(id, path, components)
    }
  } */

  def parsePath: RDFParser[SHACLPath] = (n, rdf) => {
    n match {
      case iri: IRI => Success(PredicatePath(iri))
      case bnode: BNodeId => someOf(
        inversePath,
        oneOrMorePath,
        zeroOrMorePath,
        zeroOrOnePath,
        alternativePath,
        sequencePath)(n,rdf)
      case _ => parseFail(s"Unsupported value $n for path")
    }
  }

  def inversePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNode <- objectFromPredicate(sh_inversePath)(n,rdf)
    path <- parsePath(pathNode,rdf)
  } yield InversePath(path)


  def oneOrMorePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNode <- objectFromPredicate(sh_oneOrMorePath)(n,rdf)
    path <- parsePath(pathNode,rdf)
  } yield OneOrMorePath(path)

  def zeroOrMorePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNode <- objectFromPredicate(sh_zeroOrMorePath)(n,rdf)
    path <- parsePath(pathNode,rdf)
  } yield ZeroOrMorePath(path)

  def zeroOrOnePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNode <- objectFromPredicate(sh_zeroOrOnePath)(n,rdf)
    path <- parsePath(pathNode,rdf)
  } yield ZeroOrOnePath(path)

  def alternativePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNode <- objectFromPredicate(sh_alternativePath)(n,rdf)
    pathNodes <- rdfList(pathNode,rdf)
    paths <- group(parsePath,pathNodes)(n,rdf)
  } yield AlternativePath(paths)

  def sequencePath: RDFParser[SHACLPath] = (n,rdf) => for {
    pathNodes <- rdfList(n,rdf)
    paths <- group(parsePath,pathNodes)(n,rdf)
  } yield {
    SequencePath(paths)
  }

  def components: RDFParser[Seq[Component]] =
    anyOf(
        classComponent,
        datatype,
        nodeKind,
        minCount, maxCount,
        minExclusive, maxExclusive, minInclusive, maxInclusive,
        minLength, maxLength,
        pattern, languageIn, uniqueLang,
        equals, disjoint, lessThan, lessThanOrEquals,
        or, and, not, xone, qualifiedValueShape,
        nodeComponent,
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

  def languageIn : RDFParser[LanguageIn] = (n,rdf) => for {
    rs <- rdfListForPredicate(sh_languageIn)(n,rdf)
    ls <- rs.map(n => n match {
      case StringLiteral(str) => Success(str)
      case _ => parseFail(s"Expected to be a string literal but got $n")
    }).sequence
  } yield LanguageIn(ls)

  def uniqueLang : RDFParser[UniqueLang] = (n,rdf) => for {
    b <- booleanFromPredicate(sh_uniqueLang)(n,rdf)
  } yield UniqueLang(b)

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

  def xone : RDFParser[Xone] = (n,rdf) => for {
    nodes <- rdfListForPredicate(sh_xone)(n,rdf)
    if (!nodes.isEmpty)
    shapes <- mapRDFParser(nodes,getShape)(n,rdf)
  } yield Xone(shapes)

  def not: RDFParser[Not] = (n,rdf) => for {
    shapeNode <- objectFromPredicateOptional(sh_not)(n,rdf)
    if (shapeNode.isDefined)
    shape <- getShape(shapeNode.get)(n,rdf)
  } yield Not(shape)

  def nodeComponent: RDFParser[NodeComponent] = (n, rdf) => {
    for {
     nodeShape <- objectFromPredicate(sh_node)(n,rdf)
     s <- getShape(nodeShape)(n,rdf)
    } yield {
      NodeComponent(s)
    }
  }

  def qualifiedValueShape: RDFParser[QualifiedValueShape] = (n,rdf) => for {
    obj <- objectFromPredicate(sh_qualifiedValueShape)(n,rdf)
    shape <- getShape(obj)(n,rdf)
    min <- optional(integerLiteralForPredicate(sh_qualifiedMinCount))(n,rdf)
    max <- optional(integerLiteralForPredicate(sh_qualifiedMaxCount))(n,rdf)
    disjoint <- booleanFromPredicateOptional(sh_qualifiedValueShapesDisjoint)(n,rdf)
  } yield QualifiedValueShape(shape, min,max, disjoint)

  def getShape(node: RDFNode): RDFParser[ShapeRef] = (n, rdf) =>
    /* if (parsedShapes.contains(node)) {
      val shape = parsedShapes(node)
      logger.info(s"Shape already parsed for node $node with value: $shape")
      Success(shape)
    }
    else */
    shape(node,rdf)

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
    for {
      os <- objectsFromPredicate(sh_nodeKind)(n,rdf)
      nk <- parseNodeKind(os)
    } yield {
      nk
    }
  }

  def parseNodeKind(os: Set[RDFNode]): Try[Component] = {
    os.size match {
      case 0 => parseFail("no iriObjects of nodeKind property")
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
      case n => parseFail(s"iriObjects of nodeKind property > 1. $os")
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

  def noTarget: Seq[Target] = Seq()
  def noPropertyShapes: Seq[PropertyShape] = Seq()

}
