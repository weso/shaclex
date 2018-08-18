package es.weso.shacl.converter
import scala.util._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.nodes._
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.{RDFBuilder, nodes}
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
      _ <- addPrefix("sh", sh.str)
      _ <- addPrefix("xsd", xsd.str)
      _ <- addPrefix("rdf", rdf.str)
      _ <- addPrefix("rdfs", rdfs.str)
      _ <- sequence(shacl.shapes.toList.map(shape(_)))
    } yield ()
  }

  private def shape(shape: Shape): RDFSaver[RDFNode] = shape match {
    case ns: NodeShape     => nodeShape(ns)
    case ps: PropertyShape => propertyShape(ps)
  }

  private def shapeRef(shape: ShapeRef): RDFSaver[RDFNode] = ok(shape.id)

  private def makeShapeId(v: RDFNode): RDFSaver[RDFNode] = ok(v)

  private def targets(id: RDFNode, ts: Seq[Target]): RDFSaver[Unit] =
    saveList(ts.toList, target(id))

  private def target(id: RDFNode)(t: Target): RDFSaver[Unit] = t match {
    case TargetNode(node)       => addTriple(id, sh_targetNode, node)
    case TargetClass(node)      => addTriple(id, sh_targetClass, node)
    case TargetSubjectsOf(node) => addTriple(id, sh_targetSubjectsOf, node)
    case TargetObjectsOf(node)  => addTriple(id, sh_targetObjectsOf, node)
  }

  private def propertyShapes(id: RDFNode, ts: Seq[ShapeRef]): RDFSaver[Unit] =
    saveList(ts.toList, makePropertyShape(id))

  private def makePropertyShape(id: RDFNode)(p: ShapeRef): RDFSaver[Unit] =
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
    _ <- addTriple(shapeNode, rdf_type, sh_PropertyShape)
    _ <- targets(shapeNode, ps.targets)
    _ <- propertyShapes(shapeNode, ps.propertyShapes)
    _ <- closed(shapeNode, ps.closed)
    _ <- deactivated(shapeNode, ps.deactivated)
    _ <- ignoredProperties(shapeNode, ps.ignoredProperties)
    _ <- message(shapeNode, ps.message)
    _ <- severity(shapeNode, ps.severity)
    pathNode <- makePath(ps.path)
    _ <- addTriple(shapeNode, sh_path, pathNode)
    _ <- saveList(ps.components.toList, component(shapeNode))
  } yield (shapeNode)

  private def nodeShape(n: NodeShape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(n.id)
    _ <- addTriple(shapeNode, rdf_type, sh_NodeShape)
    _ <- targets(shapeNode, n.targets)
    _ <- propertyShapes(shapeNode, n.propertyShapes)
    _ <- closed(shapeNode, n.closed)
    _ <- deactivated(shapeNode, n.deactivated)
    _ <- ignoredProperties(shapeNode, n.ignoredProperties)
    _ <- saveList(n.components, component(shapeNode))
    _ <- message(shapeNode, n.message)
    _ <- severity(shapeNode, n.severity)
  } yield shapeNode

  private def message(n: RDFNode, message: MessageMap): RDFSaver[Unit] =
    sequence(message.getRDFNodes.map(addTriple(n,sh_message,_))
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
    case UniqueLang(b) => addTriple(id, sh_uniqueLang, BooleanLiteral(b))
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
      _ <- addTriple(id, sh_xone, ls)
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

