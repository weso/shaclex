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
// import scala.util.{Failure, Success, Try}
// import cats._
import cats.data._
import cats.implicits._

object RDF2Shacl extends RDFParser with LazyLogging {

private type ShapesMap = Map[RefNode, Shape]

type PropertyGroups = Map[RefNode, PropertyGroup]

// TODO. Why this class cannot be private?
case class ParserState(
    parsedShapes: ShapesMap, 
    parsedPropertyGroups: PropertyGroups, 
    pendingNodes: List[RDFNode]
  )

private def initialState: ParserState = 
    ParserState(Map(),Map(),List())

type ShaclParser[A] = StateT[RDFParser, ParserState, A]

private def getPendingNodes: ShaclParser[List[RDFNode]] = for {
 s <- StateT.get[RDFParser,ParserState]
} yield s.pendingNodes

private def ok_s[A](x: A): ShaclParser[A] = 
  StateT.liftF(ok(x))

private def firstOf_s[A](ps: ShaclParser[A]*): ShaclParser[A] = {
  def comb(rest: ShaclParser[A], p: ShaclParser[A]): ShaclParser[A] = 
   p orElse rest
 
 val zero: ShaclParser[A] = fromRDFParser(parseFail("firstOf: none of the parsers succeeded"))
 ps.foldLeft(zero)(comb)
}

private def removePendingNode: ShaclParser[Option[RDFNode]] = for {
  ns <- getPendingNodes
  r <- ns match {
    case Nil => ok_s(none)
    case n :: rest => { 
    for {
     _ <- StateT.modify[RDFParser,ParserState](_.copy(pendingNodes = rest)) 
    } yield n.some
   }
  }
} yield r

private def addPendingNode(n: RDFNode): ShaclParser[Unit] = {
  StateT.modify(s => s.copy(pendingNodes = n :: s.pendingNodes))
}

private def addPendingNodes(ns: List[RDFNode]): ShaclParser[Unit] = {
  StateT.modify(s => s.copy(pendingNodes = ns ++ s.pendingNodes))
}

private def addShapesMap(n: RefNode, shape: Shape): ShaclParser[Unit] = {
  StateT.modify(s => 
    s.copy(parsedShapes = s.parsedShapes.updated(n,shape))
  )
}

private def addParsedPropertyGroups(n: RefNode, pg: PropertyGroup): ShaclParser[Unit] = {
  StateT.modify(s => 
    s.copy(parsedPropertyGroups = 
     s.parsedPropertyGroups.updated(n,pg))
  )
}

private def getShapesMap: ShaclParser[ShapesMap] = for {
  s <- StateT.get[RDFParser,ParserState]
} yield s.parsedShapes

private def getParsedPropertyGroups: ShaclParser[PropertyGroups] = for {
  s <- StateT.get[RDFParser,ParserState]
} yield s.parsedPropertyGroups


private def getRDF_s: ShaclParser[RDFReader] = 
  StateT.liftF(getRDF)

private def getNode_s: ShaclParser[RDFNode] = 
  StateT.liftF(getNode)

private def fromEither_s[A](e: Either[Err,A]): ShaclParser[A] =
  StateT.liftF(fromEither(e))

private def fromRDFParser[A](p: RDFParser[A]): ShaclParser[A] =
  StateT.liftF(p)

private def withNode_s[A](node: RDFNode, p: ShaclParser[A]): ShaclParser[A] = 
 StateT.applyF {
   val ff: (ParserState => RDFParser[(ParserState, A)]) => 
             (ParserState => RDFParser[(ParserState, A)]) 
     = f => (ps => withNode(node, f(ps)))
   p.runF.map(ff) 
 }

/*private def showStatus(msg: String): ShaclParser[Unit] = for {
  ps <- StateT.get[RDFParser,ParserState]
} yield {
  println(s"""
    |$msg
    |Parser state: PendingNodes: ${ps.pendingNodes.map(_.toString).mkString(",")}
    |parsedShapesMap: ${ps.parsedShapes.toString}
    |parsedPropertyGroups: ${ps.parsedPropertyGroups.keySet.map(_.toString).mkString(",")}""".stripMargin)
  ()
}*/


private def anyOf_s[A](ps: ShaclParser[A]*): ShaclParser[Seq[A]] = {
 def comb(rest: ShaclParser[Seq[A]], p: ShaclParser[A]): ShaclParser[Seq[A]] = for {
  maybe <- p.map(_.some) orElse ok_s(none[A])
  rs <- rest 
 } yield maybe match {
  case None => rs
  case Some(x) => x +: rs
 }      
 val zero: ShaclParser[Seq[A]] = ok_s(Seq())
 ps.foldLeft(zero)(comb)
}

/*  private def tryGetShacl(rdf: RDFBuilder,
                  resolveImports: Boolean): Try[Schema] =
    getShacl(rdf, resolveImports).fold(
      str => Failure(new Exception(str)),
      Success(_))
*/

private def getShaclFromRDFReader(rdf: RDFReader): ShaclParser[Schema] = {
    val pm = rdf.getPrefixMap
    for {
      sm <- shapesMap
      imports <- parseImports
      entailments <- parseEntailments
      parsedPropertyGroups <- getParsedPropertyGroups
      parsedShapeMap <- getShapesMap
    } yield Schema(
      pm = pm,
      imports = imports,
      entailments = entailments,
      shapesMap = parsedShapeMap,
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
      extendedRdf <- if (resolveImports) rdf.extendImports()
                     else Right(rdf)
      schema <- getShaclFromRDFReader(extendedRdf).run(initialState).value.run(Config(initialNode,extendedRdf))
    } yield schema._2
  }

  def getShaclReader(rdf: RDFReader): Either[String, Schema] = for {
   stateSchema <- getShaclFromRDFReader(rdf).run(initialState).value.run(Config(initialNode,rdf))
  } yield stateSchema._2

  private def shapesMap: ShaclParser[Unit] = 
    for {
      rdf <- getRDF_s
      nodeShapes <- fromEither_s(rdf.subjectsWithType(`sh:NodeShape`))
      propertyShapes <- fromEither_s(rdf.subjectsWithType(`sh:PropertyShape`))
      shapes <- fromEither_s(rdf.subjectsWithType(`sh:Shape`))
      objectsPropertyShapes <- fromEither_s(rdf.subjectsWithProperty(`sh:property`))
      allShapes = nodeShapes ++ propertyShapes ++ shapes ++ objectsPropertyShapes
      _ <- addPendingNodes(allShapes.toList)
      // _ <- showStatus("shapesMap after addPending")
      sm <- parseShapesMap
    } yield ()

  private def parseShapesMap: ShaclParser[Unit] = for {
    maybeNode <- removePendingNode
    // _ <- { println(s"Pending node to parse: $maybeNode"); ok_s(()) }
    r <- maybeNode match {
      case None => getShapesMap
      case Some(n) => for { 
       s <- withNode_s(n,shape)
       // _ <- showStatus(s"parseShapeMaps after checking shape of ${n.toString}")
       sm <- parseShapesMap
      } yield sm
    }
  } yield ()


def initialNode: RDFNode = IRI("http://internal/node")

/*private def parseShapesMap: ShaclParser[ShapesMap] = for {
  pendingNodes <- getPendingNodes
  r <- pendingNodes.size match {
    case 0 => getShapesMap  
    case _ => withNode
  }
} yield r
*/


def shape: ShaclParser[RefNode] = for {
    n <- getNode_s
    shapeRef = RefNode(n)
    parsedShapes <- getShapesMap
    v <- if (parsedShapes contains shapeRef) {
      ok_s(shapeRef)
    } else {
      for {
        shapeRef <- firstOf_s(nodeShape, propertyShape)
      } yield {
        //        parsedShapes(shapeRef) = newShape
        shapeRef
      }
    }
  } yield v

  private def parseEntailments: ShaclParser[List[IRI]] =
      for {
        rdf <- getRDF_s
        ts <- fromEither_s(rdf.triplesWithPredicate(`sh:entailment`))
        iris <- fromEither_s(sequence(ts.map(_.obj).toList.map(_.toIRI)))
      } yield iris

  private def parseImports: ShaclParser[List[IRI]] =
    for {
     rdf <- getRDF_s 
     ts <- fromEither_s(rdf.triplesWithPredicate(`owl:imports`))
     os <- fromEither_s(sequence(ts.map(_.obj).toList.map(_.toIRI)))
    } yield os

  /* private def mkId(n: RDFNode): Option[IRI] = n match {
    case iri: IRI => Some(iri)
    case _ => None
  } */

  private def nodeShape: ShaclParser[RefNode] = for {
    n <- getNode_s
    rdf <- getRDF_s
    types <- fromRDFParser(rdfTypes)
    _ <- fromRDFParser(failIf(types.contains(`sh:PropertyShape`), "Node shapes must not have rdf:type sh:PropertyShape"))
    targets <- fromRDFParser(targets)
    propertyShapes <- propertyShapes
    components <- components
    closed <- fromRDFParser(booleanFromPredicateOptional(`sh:closed`))
    deactivated <- fromRDFParser(booleanFromPredicateOptional(`sh:deactivated`))
    message <- fromRDFParser(parseMessage)
    name <- fromRDFParser(parseMessage)
    description <- fromRDFParser(parseMessage)
    group <- parsePropertyGroup
    order <- fromRDFParser(parseOrder)
    severity <- fromRDFParser(parseSeverity)
    ignoredNodes <- fromRDFParser(rdfListForPredicateOptional(`sh:ignoredProperties`))
    ignoredIRIs <- fromEither_s(nodes2iris(ignoredNodes))
    classes <- fromRDFParser(objectsFromPredicate(`sh:class`))
    shape = NodeShape(
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
    ref = RefNode(n)
    _ <- addShapesMap(ref,shape)
    // _ <- { println(s"NodeShape parsed for node ${n.toString}: ${shape.toString}"); ok_s(())}
    // _ <- showStatus(s"After nodeShape of ${n.toString}")
  } yield ref

  private def parsePropertyGroup: ShaclParser[Option[RefNode]] = for {
    maybeGroup <- fromRDFParser(objectFromPredicateOptional(`sh:group`))
    group <- maybeGroup match {
      case None => ok_s(none)
      case Some(groupNode) => {
        val ref = RefNode(groupNode)
        for {
        parsedPropGroups <- getParsedPropertyGroups
        v <- parsedPropGroups.get(ref) match {
          case Some(pg) => ok_s(Some(ref))
          case None => for {
            labels <- fromRDFParser(objectsFromPredicate(`rdfs:label`))
            order <- fromRDFParser(parseOrder)
            pg = PropertyGroup(order,labels)
            _ <- addParsedPropertyGroups(ref,pg)
          } yield Some(ref) 
         }
        } yield v
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


  private def propertyShape: ShaclParser[RefNode] = for {
    rdf <- getRDF_s
    n <- getNode_s
    types <- fromRDFParser(rdfTypes)
    _ <- fromRDFParser(failIf(types.contains(`sh:NodeShape`), "Property shapes must not have rdf:type sh:NodeShape"))
    targets <- fromRDFParser(targets)
    nodePath <- fromRDFParser(objectFromPredicate(`sh:path`))
    path <- fromRDFParser(withNode(nodePath, parsePath))
    propertyShapes <- propertyShapes
    // _ <- { println(s"Components of ${n}"); ok_s(())}
    components <- components
    closed <- fromRDFParser(booleanFromPredicateOptional(`sh:closed`))
    ignoredNodes <- fromRDFParser(rdfListForPredicateOptional(`sh:ignoredProperties`))
    deactivated <- fromRDFParser(booleanFromPredicateOptional(`sh:deactivated`))
    message <- fromRDFParser(parseMessage)
    severity <- fromRDFParser(parseSeverity)
    name <- fromRDFParser(parseMessage)
    description <- fromRDFParser(parseMessage)
    group <- parsePropertyGroup
    order <- fromRDFParser(parseOrder)
    ignoredIRIs <- fromEither_s(nodes2iris(ignoredNodes))
    ps = PropertyShape(
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
    ref = RefNode(n)
    // _ <- { println(s"Property shape: ${n}: Components: ${components.toString}"); ok_s(())} 
    _ <- addShapesMap(ref,ps)
  } yield ref

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

  private def propertyShapes: ShaclParser[List[RefNode]] = 
    for {
      ps <- fromRDFParser(objectsFromPredicate(`sh:property`))
      vs <- ps.toList.map(p => withNode_s(p, propertyShapeRef)).sequence
    } yield vs
  
  private def propertyShapeRef: ShaclParser[RefNode] = for {
    n <- getNode_s
    _ <- addPendingNode(n)
  } yield {
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

  private def components: ShaclParser[Seq[Component]] = for {
    n <- fromRDFParser(getNode)
    // _ <- {println(s"Components for node: ${n}"); ok_s(())}   
    cs1 <- fromRDFParser(anyOf(
      pattern, languageIn, uniqueLang,
      equals, disjoint, lessThan, lessThanOrEquals,
      hasValue,
      in))
    // _ <- {println(s"First round of components: ${cs1}"); ok_s(())}   
    cs2 <- fromRDFParser(anyOfLs_s(
      classComponent,
      datatype,
      nodeKind,
      minCount, maxCount,
      minExclusive, maxExclusive, minInclusive, maxInclusive,
      minLength, maxLength
    ))
    // _ <- {println(s"2nd round of components: ${cs2}"); ok_s(())}   
    cs3 <- anyOf_s(qualifiedValueShape, or, and, not, xone, nodeComponent)
  } yield { 
    val cs = cs1 ++ cs2 ++ cs3.toSeq 
    // println(s"Components: $cs")
    cs
  }

  private def anyOfLs_s[A](ps: RDFParser[List[A]]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[List[A]]): RDFParser[Seq[A]] = {
      p.biflatMap(
        _ => rest, 
        xs => for {
          rs <- rest
        } yield xs ++ rs
      ) 
    }
    val zero: RDFParser[Seq[A]] = ok(Seq())
    val vs = ps.foldLeft(zero)(comb)
    vs
  } 


  private def classComponent: RDFParser[List[Component]] = for {
    cs <- {
      parsePredicateList(`sh:class`, ClassComponent)
    }
  } yield {
    cs
  }

  private def datatype: RDFParser[List[Component]] = 
     parsePredicateIRIList(`sh:datatype`, Datatype)

  private def minInclusive : RDFParser[List[Component]] = 
    parsePredicateLiteralList(`sh:minInclusive`, MinInclusive)

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

  private def or: ShaclParser[Component] = for {
    shapeNodes <- fromRDFParser(rdfListForPredicate(`sh:or`))
    shapes <- mapShaclParser(shapeNodes.toList, shapeRefConst)
  } yield Or(shapes)


  private def and: ShaclParser[Component] = for {
    nodes <- fromRDFParser(rdfListForPredicate(`sh:and`))
    shapes <- mapShaclParser(nodes, shapeRefConst)
  } yield And(shapes)

  private def xone: ShaclParser[Component] = for {
    nodes <- fromRDFParser(rdfListForPredicate(`sh:xone`))
    shapes <- mapShaclParser(nodes, shapeRefConst)
  } yield Xone(shapes)

  // TODO: Check if this must take into account that not is optional...
  private def not: ShaclParser[Component] = for {
    shapeNode <- fromRDFParser(objectFromPredicate(`sh:not`))
    sref <- withNode_s(shapeNode, shapeRef)
  } yield Not(sref)

  private def nodeComponent: ShaclParser[Component] = 
    for {
      nodeShape <- fromRDFParser(objectFromPredicate(`sh:node`))
      sref <- withNode_s(nodeShape, shapeRef) 
    } yield NodeComponent(sref)

  private def qualifiedValueShape: ShaclParser[Component] = for {
    obj <- fromRDFParser(objectFromPredicate(`sh:qualifiedValueShape`))
    sref <- withNode_s(obj, shapeRef) 
    min <- fromRDFParser(optional(integerLiteralForPredicate(`sh:qualifiedMinCount`)))
    max <- fromRDFParser(optional(integerLiteralForPredicate(`sh:qualifiedMaxCount`)))
    disjoint <- fromRDFParser(booleanFromPredicateOptional(`sh:qualifiedValueShapesDisjoint`))
  } yield QualifiedValueShape(sref, min, max, disjoint)

  private def shapeRef: ShaclParser[RefNode] = for { 
    n <- getNode_s 
    _ <- addPendingNode(n)
  } yield RefNode(n)

  private def shapeRefConst(sref: RDFNode): ShaclParser[RefNode] = 
    withNode_s(sref, shapeRef) 

  def mapShaclParser[A, B](ls: List[A], p: A => ShaclParser[B]): ShaclParser[List[B]] = {
      ls.map(v => p(v)).sequence[ShaclParser,B]
  }
  

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

//  private def noTarget: Seq[Target] = Seq()
//  private def noPropertyShapes: Seq[PropertyShape] = Seq()

}
