package es.weso.shacl.converter

import scala.util._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.nodes._
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf._
import es.weso.rdf.saver.RDFSaver
import es.weso.shacl._
import es.weso.shacl.report.Severity

class Shacl2RDF extends RDFSaver with LazyLogging {

  def serialize(shacl: Schema, format: String, builder: RDFBuilder): Either[String, String] = {
    val rdf: RDFBuilder = toRDF(shacl, builder)
    rdf.serialize(format)
  }

  def toRDF(shacl: Schema, initial: RDFBuilder): RDFBuilder = {
    val result = schema(shacl).run(initial)
    result.value._1
  }

  private def schema(shacl: Schema): RDFSaver[Unit] = {
    for {
      _ <- addPrefix("sh", sh)
      _ <- addPrefix("xsd", xsd)
      _ <- addPrefix("rdf", rdf)
      _ <- addPrefix("rdfs", rdfs)
      _ <- sequence(shacl.shapes.toList.map(shape(_)))
      _ <- sequence(shacl.propertyGroups.toList.map(propertyGroup))
    } yield ()
  }

  private def shape(shape: Shape): RDFSaver[RDFNode] = shape match {
    case ns: NodeShape     => nodeShape(ns)
    case ps: PropertyShape => propertyShape(ps)
  }

  private def propertyGroup(pair: (RefNode, PropertyGroup)): RDFSaver[RDFNode] = {
    val (ref,pg) = pair
    val node = ref.id
    for {
     _ <- order(node,pg.order)
     _ <- labels(node, pg.label)
    } yield node
  }

  private def labels(node: RDFNode, labels: Set[RDFNode]): RDFSaver[Unit] =
    sequence(
      labels.toList.map(lbl => addTriple(node,`rdfs:label`,lbl))
    ).map(_ => ())

  private def shapeRef(shape: RefNode): RDFSaver[RDFNode] = ok(shape.id)

  private def makeShapeId(v: RDFNode): RDFSaver[RDFNode] = ok(v)

  private def targets(id: RDFNode, ts: Seq[Target]): RDFSaver[Unit] =
    saveList(ts.toList, target(id))

  private def target(id: RDFNode)(t: Target): RDFSaver[Unit] = t match {
    case TargetNode(node)       => addTriple(id, sh_targetNode, node)
    case TargetClass(node)      => addTriple(id, sh_targetClass, node)
    case TargetSubjectsOf(node) => addTriple(id, sh_targetSubjectsOf, node)
    case TargetObjectsOf(node)  => addTriple(id, sh_targetObjectsOf, node)
  }

  private def propertyShapes(id: RDFNode, ts: Seq[RefNode]): RDFSaver[Unit] =
    saveList(ts.toList, makePropertyShape(id))

  private def makePropertyShape(id: RDFNode)(p: RefNode): RDFSaver[Unit] =
    for {
      node <- ok(p.id) // propertyShape(p)
      _    <- addTriple(id, sh_property, node)
    } yield ()

  private def closed(id: RDFNode, b: Boolean): RDFSaver[Unit] =
    if (b)
      addTriple(id, sh_closed, BooleanLiteral(b))
    else ok(())

  private def deactivated(id: RDFNode, b: Boolean): RDFSaver[Unit] =
    if (b)
      addTriple(id, sh_deactivated, BooleanLiteral(b))
    else ok(())

  private def ignoredProperties(id: RDFNode, ignored: List[IRI]): RDFSaver[Unit] =
    if (!ignored.isEmpty) {
      for {
        nodeList <- saveToRDFList(ignored, (iri: IRI) => State.pure(iri))
        _ <- addTriple(id, sh_ignoredProperties, nodeList)
      } yield ()
    } else
      State.pure(())

  private def propertyShape(ps: PropertyShape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(ps.id)
    _ <- addTriple(shapeNode, `rdf:type`, `sh:PropertyShape`)
    _ <- targets(shapeNode, ps.targets)
    _ <- propertyShapes(shapeNode, ps.propertyShapes)
    _ <- closed(shapeNode, ps.closed)
    _ <- deactivated(shapeNode, ps.deactivated)
    _ <- ignoredProperties(shapeNode, ps.ignoredProperties)
    _ <- messageMap(shapeNode, ps.message, sh_message)
    _ <- messageMap(shapeNode, ps.message, sh_name)
    _ <- messageMap(shapeNode, ps.message, sh_description)
    _ <- order(shapeNode,ps.order)
    _ <- group(shapeNode,ps.group)
    _ <- severity(shapeNode, ps.severity)
    pathNode <- makePath(ps.path)
    _ <- addTriple(shapeNode, sh_path, pathNode)
    _ <- saveList(ps.components.toList, component(shapeNode))
  } yield (shapeNode)

  private def nodeShape(n: NodeShape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(n.id)
    _ <- addTriple(shapeNode, `rdf:type`, `sh:NodeShape`)
    _ <- targets(shapeNode, n.targets)
    _ <- propertyShapes(shapeNode, n.propertyShapes)
    _ <- closed(shapeNode, n.closed)
    _ <- deactivated(shapeNode, n.deactivated)
    _ <- ignoredProperties(shapeNode, n.ignoredProperties)
    _ <- saveList(n.components, component(shapeNode))
    _ <- messageMap(shapeNode, n.message, sh_message)
    _ <- messageMap(shapeNode, n.name, sh_name)
    _ <- messageMap(shapeNode, n.name, sh_description)
    _ <- severity(shapeNode, n.severity)
    _ <- order(shapeNode,n.order)
    _ <- group(shapeNode,n.group)
  } yield shapeNode

  private def order(n: RDFNode, maybeValue: Option[DecimalLiteral]): RDFSaver[Unit] =
    maybeValue match {
      case None => ok(())
      case Some(value) => addTriple(n,sh_order,value)
    }

  private def group(n: RDFNode, maybeValue: Option[RefNode]): RDFSaver[Unit] =
    maybeValue match {
      case None => ok(())
      case Some(pg) => {
        addTriple(n, sh_group, pg.id)
      }
    }

  private def messageMap(n: RDFNode, message: MessageMap, pred: IRI): RDFSaver[Unit] =
    sequence(message.getRDFNodes.map(addTriple(n,pred,_))
    ).map(_ => ())

  private def severity(n: RDFNode, severity: Option[Severity]): RDFSaver[Unit] =
    severity match {
      case None => ok(())
      case Some(s) => addTriple(n,sh_severity,s.toIRI)
    }

  private def component(id: RDFNode)(c: Component): RDFSaver[Unit] = c match {
    case ClassComponent(v) => addTriple(id, sh_class, v)
    case Datatype(iri) => addTriple(id, sh_datatype, iri)
    case NodeKind(value) => addTriple(id, sh_nodeKind, value.id)
    case MinCount(n) => addTriple(id, sh_minCount, IntegerLiteral(n))
    case MaxCount(n) => addTriple(id, sh_maxCount, IntegerLiteral(n))
    case MinExclusive(v) => addTriple(id, sh_minExclusive, v)
    case MinInclusive(v) => addTriple(id, sh_minInclusive, v)
    case MaxExclusive(v) => addTriple(id, sh_maxExclusive, v)
    case MaxInclusive(v) => addTriple(id, sh_maxInclusive, v)
    case MinLength(n) => addTriple(id, sh_minLength, IntegerLiteral(n))
    case MaxLength(n) => addTriple(id, sh_maxLength, IntegerLiteral(n))
    case Pattern(p, flags) => addTriple(id, sh_pattern, StringLiteral(p)) >>
      (flags match {
        case Some(f) => addTriple(id, sh_flags, StringLiteral(f))
        case None => State.pure(())
      })
    case UniqueLang(b) => addTriple(id, `sh:uniqueLang`, BooleanLiteral(b))
    case LanguageIn(langs) => for {
      ls <- saveToRDFList(langs, (lang: String) => State.pure(StringLiteral(lang)))
      _ <- addTriple(id, sh_languageIn, ls)
    } yield ()
    case Equals(p) => addTriple(id, sh_equals, p)
    case Disjoint(p) => addTriple(id, sh_disjoint, p)
    case LessThan(p) => addTriple(id, sh_lessThan, p)
    case LessThanOrEquals(p) => addTriple(id, sh_lessThanOrEquals, p)
    case And(shapes) => for {
      ls <- saveToRDFList(shapes, shapeRef)
      _ <- addTriple(id, sh_and, ls)
    } yield ()
    case Or(shapes) => for {
      ls <- saveToRDFList(shapes, shapeRef)
      _ <- addTriple(id, sh_or, ls)
    } yield ()
    case Xone(shapes) => for {
      ls <- saveToRDFList(shapes, shapeRef)
      _ <- addTriple(id, `sh:xone`, ls)
    } yield ()
    case QualifiedValueShape(s, min, max, disjoint) => for {
      nodeShape <- shapeRef(s)
      _ <- addTriple(id, sh_qualifiedValueShape, nodeShape)
      _ <- maybeAddTriple(id, sh_qualifiedMinCount, min.map(IntegerLiteral(_)))
      _ <- maybeAddTriple(id, sh_qualifiedMaxCount, max.map(IntegerLiteral(_)))
      _ <- maybeAddTriple(id, sh_qualifiedValueShapesDisjoint, disjoint.map(BooleanLiteral(_)))
    } yield ()
    case Not(s) => for {
      nodeS <- shapeRef(s)
      _ <- addTriple(id, sh_not, nodeS)
    } yield ()
    case Closed(b, ignoredPs) => for {
      _ <- addTriple(id, sh_closed, BooleanLiteral(b))
      nodeList <- saveToRDFList(ignoredPs, (iri: IRI) => State.pure(iri))
      _ <- addTriple(id, sh_ignoredProperties, nodeList)
    } yield ()
    case NodeComponent(s) => for {
      nodeS <- shapeRef(s)
      _ <- addTriple(id, sh_node, nodeS)
    } yield ()
    case HasValue(v) => addTriple(id, sh_hasValue, v.rdfNode)
    case In(vs) => for {
      nodeLs <- saveToRDFList(vs, (v: Value) => State.pure(v.rdfNode))
      _ <- addTriple(id, sh_in, nodeLs)
    } yield ()
  }

}

