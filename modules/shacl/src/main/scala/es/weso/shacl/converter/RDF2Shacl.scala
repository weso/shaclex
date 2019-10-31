package es.weso.shacl.converter

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES._
import es.weso.rdf.{RDFBuilder, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.parser.RDFParser
import es.weso.rdf.path._
import es.weso.shacl.SHACLPrefixes._
import es.weso.shacl._
import es.weso.shacl.report._
import es.weso.utils.EitherUtils._
import scala.util.{Failure, Success, Try}
import cats._
import cats.data._
import cats.implicits._

object RDF2Shacl extends RDFParser with LazyLogging {

  // Keep track of parsed shapes
  // TODO: Refactor this code to use a StateT
  private val parsedShapes = collection.mutable.Map[RefNode, Shape]()

  // TODO: Refactor this code to avoid imperative style
  // private var pendingNodes = List[RDFNode]()
  private type ShapesMap = Map[RefNode, Shape]

  private val parsedPropertyGroups = collection.mutable.Map[RefNode, PropertyGroup]()

type PropertyGroups = Map[RefNode, PropertyGroup]

case class ParserState(
    shapesMap: ShapesMap, 
    parsedPropertyGroups: PropertyGroups, 
    pendingNodes: List[RDFNode]
  )

def initialState(pendingNodes: List[RDFNode]): ParserState = 
    ParserState(Map(),Map(),pendingNodes)

type ShaclParser[A] = StateT[RDFParser, ParserState, A]

private def getPendingNodes: ShaclParser[List[RDFNode]] = for {
 s <- StateT.get[RDFParser,ParserState]
} yield s.pendingNodes

private def removePendingNode: ShaclParser[Option[RDFNode]] = for {
  ns <- getPendingNodes
  r <- ns match {
    case Nil => ok(None)
    case n :: rest => StateT.modify(s: ParserState => s.copy(pendingNodes = rest)) >> ok(Some(n))
  }
} yield r

private def addPendingNode(n: RDFNode): ShaclParser[Unit] = {
  StateT.modify(s => s.copy(pendingNodes = n :: s.pendingNodes))
}

private def getShapesMap: ShaclParser[ShapesMap] = for {
  s <- StateT.get[RDFParser,ParserState]
} yield s.shapesMap

private def fromParser[A](parser: RDFParser[A]): ShaclParser[A] =
  StateT.lift(parser)

/*  private def tryGetShacl(rdf: RDFBuilder,
                  resolveImports: Boolean): Try[Schema] =
    getShacl(rdf, resolveImports).fold(
      str => Failure(new Exception(str)),
      Success(_))
*/


private def getShaclFromRDFReader(rdf: RDFReader): Either[String,Schema] = {
    val pm = rdf.getPrefixMap
    for {
      shapesMap <- shapesMap(rdf)
      imports <- parseImports(rdf)
      entailments <- parseEntailments(rdf)
    } yield Schema(
      pm = pm,
      imports = imports,
      entailments = entailments,
      shapesMap = shapesMap,
      propertyGroups = parsedPropertyGroups.toMap
    )
  }

  /**
   * Parses RDF content and obtains a SHACL Schema and a PrefixMap
   */
  def getShacl(rdf: RDFBuilder,
               resolveImports: Boolean = true
              ): Either[String, Schema] = {
    for {
      extendedRdf <-
        if (resolveImports) rdf.extendImports()
        else Right(rdf)
      schema <- getShaclFromRDFReader(extendedRdf)
    } yield schema
  }

  private def shapesMap(rdf: RDFReader): Either[String, ShapesMap] = {
    parsedShapes.clear()
    parsedPropertyGroups.clear()
    for {
      nodeShapes <- rdf.subjectsWithType(`sh:NodeShape`)
      propertyShapes <- rdf.subjectsWithType(`sh:PropertyShape`)
      shapes <- rdf.subjectsWithType(`sh:Shape`)
      objectsPropertyShapes <- rdf.subjectsWithProperty(`sh:property`)
      sm <-{
        val allShapes = nodeShapes ++ propertyShapes ++ shapes ++ objectsPropertyShapes
        pendingNodes = allShapes.toList
        parseShapesMap.run(initialState(pendingNodes))
        .value
        .run(Config(initialNode, rdf))
        .map(_._2)
      }
    } yield sm
   }

  private def parseShapesMap: ShaclParser[ShapesMap] = for {
    maybeNode <- removePendingNode
    r <- maybeNode match {
      case None => getShapesMap
      case Some(n) => fromParser(withNode(n,shape))
    }
  } yield r


def initialNode: RDFNode = IRI("http://internal/node")

/*private def parseShapesMap: ShaclParser[ShapesMap] = for {
  pendingNodes <- getPendingNodes
  r <- pendingNodes.size match {
    case 0 => getShapesMap  
    case _ => withNode
  }
} yield r
*/


def shape: RDFParser[RefNode] = for {
    n <- getNode
    shapeRef = RefNode(n)
    v <- if (parsedShapes contains shapeRef) {
      parseOk(shapeRef)
    } else {
      for {
        shapeRef <- firstOf(nodeShape, propertyShape)
      } yield {
        //        parsedShapes(shapeRef) = newShape
        shapeRef
      }
    }
  } yield v

  private def parseEntailments(rdf: RDFReader): Either[String, List[IRI]] =
      for {
        ts <- rdf.triplesWithPredicate(`sh:entailment`)
        iris <- sequence(ts.map(_.obj).toList.map(_.toIRI))
      } yield iris

  private def parseImports(rdf: RDFReader): Either[String, List[IRI]] =
    for {
     ts <- rdf.triplesWithPredicate(`owl:imports`)
     os <- sequence(ts.map(_.obj).toList.map(_.toIRI))
    } yield os

  private def mkId(n: RDFNode): Option[IRI] = n match {
    case iri: IRI => Some(iri)
    case _ => None
  }

  private def nodeShape: RDFParser[RefNode] = for {
    n <- getNode
    rdf <- getRDF
    types <- rdfTypes
    _ <- failIf(types.contains(`sh:PropertyShape`), "Node shapes must not have rdf:type sh:PropertyShape")
    targets <- targets
    propertyShapes <- propertyShapes
    components <- components
    closed <- booleanFromPredicateOptional(`sh:closed`)
    deactivated <- booleanFromPredicateOptional(`sh:deactivated`)
    message <- parseMessage
    name <- parseMessage
    description <- parseMessage
    group <- parsePropertyGroup
    order <- parseOrder
    severity <- parseSeverity
    ignoredNodes <- rdfListForPredicateOptional(`sh:ignoredProperties`)
    ignoredIRIs <- fromEither(nodes2iris(ignoredNodes))
    classes <- objectsFromPredicate(`sh:class`)
  } yield {
    val shape: Shape = NodeShape(
      id = n,
      components = components.toList,
      targets = targets,
      propertyShapes = propertyShapes,
      closed = closed.getOrElse(false),
      ignoredProperties = ignoredIRIs,
      deactivated = deactivated.getOrElse(false),
      message = message,
      severity = severity,
      name = name,
      description = description,
      group = group,
      order = order,
      sourceIRI = rdf.sourceIRI
    )
    val sref = RefNode(n)
    parsedShapes += (sref -> shape)
    sref
  }

  private def parsePropertyGroup: RDFParser[Option[RefNode]] = for {
    maybeGroup <- objectFromPredicateOptional(`sh:group`)
    group <- maybeGroup match {
      case None => ok(None)
      case Some(groupNode) => {
        val ref = RefNode(groupNode)
        parsedPropertyGroups.get(ref) match {
        case Some(pg) => ok(Some(ref))
        case None => for {
         labels <- objectsFromPredicate(`rdfs:label`)
         order <- parseOrder
        } yield {
          val pg = PropertyGroup(order,labels)
          parsedPropertyGroups += (ref -> pg)
          Some(ref)
        }
       }
      }
    }
  } yield group

  private def parseOrder: RDFParser[Option[DecimalLiteral]] = for {
    maybeOrder <- decimalLiteralFromPredicateOptional(`sh:order`)
  } yield maybeOrder

  private def parseSeverity: RDFParser[Option[Severity]] = for {
    maybeIri <- iriFromPredicateOptional(`sh:severity`)
  } yield maybeIri match {
      case Some(`sh:Violation`) => Some(ViolationSeverity)
      case Some(`sh:Warning`) => Some(WarningSeverity)
      case Some(`sh:Info`) => Some(InfoSeverity)
      case Some(iri) => Some(GenericSeverity(iri))
      case None => None
  }

  private def parseMessage: RDFParser[MessageMap] = for {
    nodes <- objectsFromPredicate(`sh:message`)
    map <- cnvMessages(nodes)
  } yield map

  private def cnvMessages(ns: Set[RDFNode]): RDFParser[MessageMap] = 
    fromEither(MessageMap.fromRDFNodes(ns.toList))


  private def propertyShape: RDFParser[RefNode] = for {
    rdf <- getRDF
    n <- getNode
    types <- rdfTypes
    _ <- failIf(types.contains(`sh:NodeShape`), "Property shapes must not have rdf:type sh:NodeShape")
    targets <- targets
    nodePath <- objectFromPredicate(`sh:path`)
    path <- withNode(nodePath, parsePath)
    propertyShapes <- propertyShapes
    components <- components
    closed <- booleanFromPredicateOptional(`sh:closed`)
    ignoredNodes <- rdfListForPredicateOptional(`sh:ignoredProperties`)
    deactivated <- booleanFromPredicateOptional(`sh:deactivated`)
    message <- parseMessage
    severity <- parseSeverity
    name <- parseMessage
    description <- parseMessage
    group <- parsePropertyGroup
    order <- parseOrder
    ignoredIRIs <- fromEither(nodes2iris(ignoredNodes))
  } yield {
    val ps = PropertyShape(
      id = n,
      path = path,
      components = components.toList,
      targets = targets,
      propertyShapes = propertyShapes,
      closed = closed.getOrElse(false),
      ignoredProperties = ignoredIRIs,
      deactivated = deactivated.getOrElse(false),
      message = message,
      severity = severity,
      name = name,
      description = description,
      order = order,
      group = group,
      sourceIRI = rdf.sourceIRI,
      annotations = List()  // TODO: Annotations should contain the values for other predicates associated with a given node
    )

    val sref = RefNode(n)
    parsedShapes += (sref -> ps)
    sref
  }

  private def targets: RDFParser[Seq[Target]] =
    combineAll(
      targetNodes,
      targetClasses,
      implicitTargetClass,
      targetSubjectsOf,
      targetObjectsOf)

  private def targetNodes: RDFParser[Seq[Target]] = 
    for {
      ns <- objectsFromPredicate(`sh:targetNode`)
      vs <- fromEither(sequenceEither(ns.toList.map(mkTargetNode)))
    } yield vs

  private def targetClasses: RDFParser[Seq[Target]] = 
    for {
      ns <- objectsFromPredicate(`sh:targetClass`)
      vs <- fromEither(sequenceEither(ns.toList.map(mkTargetClass)))
    } yield vs

  private def implicitTargetClass: RDFParser[Seq[Target]] = 
    for {
     rdf <- getRDF 
     n <- getNode
     ts <- fromEither(rdf.triplesWithSubjectPredicate(n, `rdf:type`))
     shapeTypes = ts.map(_.obj)
     rdfs_Class = rdfs + "Class"
     r <- fromEither(if (shapeTypes.contains(rdfs_Class))
      mkTargetClass(n).map(Seq(_))
    else
      Right(Seq()))
   } yield r

  private def targetSubjectsOf: RDFParser[Seq[Target]] = 
    for {
      ns <- objectsFromPredicate(`sh:targetSubjectsOf`)
      vs <- fromEither(sequenceEither(ns.toList.map(mkTargetSubjectsOf)))
    } yield vs

  private def targetObjectsOf: RDFParser[Seq[Target]] = 
    for {
      ns <- objectsFromPredicate(`sh:targetObjectsOf`)
      vs <- fromEither(sequenceEither(ns.toList.map(mkTargetObjectsOf)))
    } yield vs

  private def mkTargetNode(n: RDFNode): Either[String, TargetNode] =
    Right(TargetNode(n))

  private def mkTargetClass(n: RDFNode): Either[String, TargetClass] =
    Right(TargetClass(n))

  private def mkTargetSubjectsOf(n: RDFNode): Either[String, TargetSubjectsOf] = n match {
    case i: IRI => Right(TargetSubjectsOf(i))
    case _ => Left(s"targetSubjectsOf requires an IRI. Obtained $n")
  }

  private def mkTargetObjectsOf(n: RDFNode): Either[String, TargetObjectsOf] = n match {
    case i: IRI => Right(TargetObjectsOf(i))
    case _ => Left(s"targetObjectsOf requires an IRI. Obtained $n")
  }

  private def propertyShapes: RDFParser[List[RefNode]] = 
    for {
      ps <- objectsFromPredicate(`sh:property`)
      vs <- ls2Parser(ps.toList.map(p => withNode(p, propertyShapeRef)))
    } yield vs
  
  private def ls2Parser[A](ls: List[RDFParser[A]]): RDFParser[List[A]] = {
    ls.sequence
  }

  private def propertyShapeRef: ShaclParser[RefNode] = for {
    n <- getNode
  } yield {
    pendingNodes = n :: pendingNodes
    RefNode(n)
  }


  private def parsePath: RDFParser[SHACLPath] = for {
    n <- getNode
    v <- n match {
      case iri: IRI => ok(PredicatePath(iri))
      case bnode: BNode => someOf(
        oneOrMorePath,
        zeroOrMorePath,
        zeroOrOnePath,
        alternativePath,
        sequencePath,
        inversePath
      )
      case _ => parseFail(s"Unsupported value $n for path")
    }
  } yield v

  private def inversePath: RDFParser[SHACLPath] = for {
    pathNode <- objectFromPredicate(`sh:inversePath`)
    path <- withNode(pathNode,parsePath)
  } yield InversePath(path)

  private def oneOrMorePath: RDFParser[SHACLPath] = for {
    pathNode <- objectFromPredicate(`sh:oneOrMorePath`)
    path <- withNode(pathNode,parsePath) 
  } yield OneOrMorePath(path)

  private def zeroOrMorePath: RDFParser[SHACLPath] = for {
    pathNode <- objectFromPredicate(`sh:zeroOrMorePath`)
    path <- withNode(pathNode,parsePath)
  } yield ZeroOrMorePath(path)

  private def zeroOrOnePath: RDFParser[SHACLPath] = for {
    pathNode <- objectFromPredicate(`sh:zeroOrOnePath`)
    path <- withNode(pathNode, parsePath) 
  } yield ZeroOrOnePath(path)

  private def alternativePath: RDFParser[SHACLPath] = for {
    pathNode <- objectFromPredicate(`sh:alternativePath`)
    pathNodes <- withNode(pathNode, rdfList) 
    paths <- group(parsePath, pathNodes)
  } yield AlternativePath(paths)

  private def sequencePath: RDFParser[SHACLPath] = for {
    pathNodes <- rdfList
    paths <- group(parsePath, pathNodes)
  } yield {
    SequencePath(paths)
  }

  private def components: RDFParser[Seq[Component]] = for {
    cs1 <- anyOf(
      pattern, languageIn, uniqueLang,
      equals, disjoint, lessThan, lessThanOrEquals,
      or, and, not, xone, qualifiedValueShape,
      nodeComponent,
      hasValue,
      in)
    cs2 <- anyOfLs(
      classComponent,
      datatype,
      nodeKind,
      minCount, maxCount,
      minExclusive, maxExclusive, minInclusive, maxInclusive,
      minLength, maxLength
    )
  } yield cs1 ++ cs2

  private def classComponent: RDFParser[List[Component]] = for {
    cs <- {
      parsePredicateList(`sh:class`, ClassComponent)
    }
  } yield {
    cs
  }

  private def datatype: RDFParser[List[Component]] = 
     parsePredicateIRIList(`sh:datatype`, Datatype)

  private def minInclusive : RDFParser[List[Component]] = parsePredicateLiteralList(`sh:minInclusive`, MinInclusive)

  private def maxInclusive : RDFParser[List[Component]] = parsePredicateLiteralList(`sh:maxInclusive`, MaxInclusive)

  private def minExclusive : RDFParser[List[Component]] = parsePredicateLiteralList(`sh:minExclusive`, MinExclusive)

  private def maxExclusive:  RDFParser[List[Component]] = parsePredicateLiteralList(`sh:maxExclusive`, MaxExclusive)

  private def minLength: RDFParser[List[Component]] = parsePredicateIntList(`sh:minLength`, MinLength)

  private def maxLength : RDFParser[List[Component]] = parsePredicateIntList(`sh:maxLength`, MaxLength)

  private def pattern: RDFParser[Component] = for {
    pat <- stringFromPredicate(`sh:pattern`)
    flags <- stringFromPredicateOptional(`sh:flags`)
  } yield Pattern(pat, flags)

  private def languageIn: RDFParser[Component] = for {
    rs <- rdfListForPredicate(`sh:languageIn`)
    ls <- fromEither(sequenceEither(rs.map(n => n match {
      case StringLiteral(str) => Right(str)
      case _ => Left(s"Expected to be a string literal but got $n")
    })))
  } yield LanguageIn(ls)

  private def uniqueLang: RDFParser[Component] = for {
    b <- booleanFromPredicate(`sh:uniqueLang`)
  } yield UniqueLang(b)

  private def equals = parsePredicateComparison(`sh:equals`, Equals)

  private def disjoint = parsePredicateComparison(`sh:disjoint`, Disjoint)

  private def lessThan = parsePredicateComparison(`sh:lessThan`, LessThan)

  private def lessThanOrEquals = parsePredicateComparison(`sh:lessThanOrEquals`, LessThanOrEquals)

  private def parsePredicateComparison(pred: IRI, mkComp: IRI => Component): RDFParser[Component] = for {
    p <- iriFromPredicate(pred)
  } yield mkComp(p)

  private def or: RDFParser[Component] = for {
    shapeNodes <- rdfListForPredicate(`sh:or`)
    shapes <- mapRDFParser(shapeNodes.toList, shapeRefConst)
  } yield Or(shapes)

  private def and: RDFParser[Component] = for {
    nodes <- rdfListForPredicate(`sh:and`)
    shapes <- mapRDFParser(nodes, shapeRefConst)
  } yield And(shapes)

  private def xone: RDFParser[Component] = for {
    nodes <- rdfListForPredicate(`sh:xone`)
    shapes <- mapRDFParser(nodes, shapeRefConst)
  } yield Xone(shapes)

  // TODO: Check if this must take into account that not is optional...
  private def not: RDFParser[Component] = for {
    shapeNode <- objectFromPredicate(`sh:not`)
    sref <- withNode(shapeNode, shapeRef)
  } yield Not(sref)

  private def nodeComponent: RDFParser[Component] = 
    for {
      nodeShape <- objectFromPredicate(`sh:node`)
      sref <- withNode(nodeShape, shapeRef) 
    } yield NodeComponent(sref)

  private def qualifiedValueShape: RDFParser[Component] = for {
    obj <- objectFromPredicate(`sh:qualifiedValueShape`)
    sref <- withNode(obj, shapeRef) 
    min <- optional(integerLiteralForPredicate(`sh:qualifiedMinCount`))
    max <- optional(integerLiteralForPredicate(`sh:qualifiedMaxCount`))
    disjoint <- booleanFromPredicateOptional(`sh:qualifiedValueShapesDisjoint`)
  } yield QualifiedValueShape(sref, min, max, disjoint)

  private def shapeRef: RDFParser[RefNode] = getNode >>= (n => 
   {
    pendingNodes = n :: pendingNodes
    parseOk(RefNode(n))
  })

  private def shapeRefConst(sref: RDFNode): RDFParser[RefNode] = 
    withNode(sref, shapeRef) 

  private def minCount : RDFParser[List[Component]] = parsePredicateIntList(`sh:minCount`, MinCount)
  private def maxCount : RDFParser[List[Component]] = parsePredicateIntList(`sh:maxCount`, MaxCount)

  private def hasValue: RDFParser[Component] = 
    for {
      o <- objectFromPredicate(`sh:hasValue`)
      v <- fromEither(node2Value(o))
    } yield HasValue(v)

  private def in: RDFParser[Component] = 
    for {
      ns <- rdfListForPredicate(`sh:in`)
      vs <- fromEither(convert2Values(ns.map(node2Value(_))))
    } yield In(vs)

  private def node2Value(n: RDFNode): Either[String, Value] = {
    n match {
      case i: IRI => Right(IRIValue(i))
      case l: Literal => Right(LiteralValue(l))
      case _ => Left(s"Element $n must be a IRI or a Literal to be part of sh:in")
    }
  }

  private def convert2Values[A](cs: List[Either[String, A]]): Either[String, List[A]] = {
    if (cs.isEmpty)
      Left("The list of values associated with sh:in must not be empty")
    else {
      sequenceEither(cs)
    }
  }

  private def nodeKind: RDFParser[List[Component]] = 
    for {
      os <- objectsFromPredicate(`sh:nodeKind`)
      nk <- fromEither(parseNodeKind(os))
    } yield List(nk)

  private def parseNodeKind(os: Set[RDFNode]): Either[String, Component] = {
    os.size match {
      case 0 => Left("no iriObjects of nodeKind property")
      case 1 => {
        os.head match {
          case nk: IRI => nk match {
            case `sh:IRI` => Right(NodeKind(IRIKind))
            case `sh:BlankNode` => Right(NodeKind(BlankNodeKind))
            case `sh:Literal` => Right(NodeKind(LiteralKind))
            case `sh:BlankNodeOrLiteral` => Right(NodeKind(BlankNodeOrLiteral))
            case `sh:BlankNodeOrIRI` => Right(NodeKind(BlankNodeOrIRI))
            case `sh:IRIOrLiteral` => Right(NodeKind(IRIOrLiteral))
            case x => {
              logger.error(s"incorrect value of nodeKind property $x")
              Left(s"incorrect value of nodeKind property $x")
            }
          }
          case x => {
            logger.error(s"incorrect value of nodeKind property $x")
            Left(s"incorrect value of nodeKind property $x")
          }
        }
      }
      case n => Left(s"iriObjects of nodeKind property > 1. $os")
    }
  }

  private def noTarget: Seq[Target] = Seq()
  private def noPropertyShapes: Seq[PropertyShape] = Seq()

}
