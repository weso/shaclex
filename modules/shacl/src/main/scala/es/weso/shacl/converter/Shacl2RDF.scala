package es.weso.shacl.converter
import scala.util._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.nodes._
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES.{ sh => _, _ }
import es.weso.rdf.jena._
import es.weso.rdf.path._
import es.weso.shacl._

class Shacl2RDF extends RDFSaver with LazyLogging {

  def serialize(shacl: Schema, format: String): Either[String, String] = {
    val rdf: RDFAsJenaModel = toRDF(shacl, RDFAsJenaModel.empty)
    Right(rdf.serialize(format))
  }

  def toRDF(shacl: Schema, initial: RDFAsJenaModel): RDFAsJenaModel = {
    val result = schema(shacl).run(initial)
    result.value._1
  }

  def schema(shacl: Schema): RDFSaver[Unit] = {
    val rs = shacl.shapes.toList.map(shape(_))
    for {
      _ <- addPrefix("sh", sh.str)
      _ <- addPrefix("xsd", xsd.str)
      _ <- addPrefix("rdf", rdf.str)
      _ <- addPrefix("rdfs", rdfs.str)
      _ <- rs.sequence
    } yield ()
  }

  def shape(shape: Shape): RDFSaver[RDFNode] = shape match {
    case ns: NodeShape => nodeShape(ns)
    case ps: PropertyShape => propertyShape(ps)
  }

  def shapeRef(shape: ShapeRef): RDFSaver[RDFNode] = ok(shape.id)

  def makeShapeId(v: RDFNode): RDFSaver[RDFNode] = ok(v)

  def targets(id: RDFNode, ts: Seq[Target]): RDFSaver[Unit] =
    saveList(ts.toList, target(id))

  def target(id: RDFNode)(t: Target): RDFSaver[Unit] = t match {
    case TargetNode(node) => addTriple(id, sh_targetNode, node)
    case TargetClass(node) => addTriple(id, sh_targetClass, node)
    case TargetSubjectsOf(node) => addTriple(id, sh_targetSubjectsOf, node)
    case TargetObjectsOf(node) => addTriple(id, sh_targetObjectsOf, node)
  }

  def propertyShapes(id: RDFNode, ts: Seq[ShapeRef]): RDFSaver[Unit] =
    saveList(ts.toList, makePropertyShape(id))

  def makePropertyShape(id: RDFNode)(p: ShapeRef): RDFSaver[Unit] = for {
    node <- ok(p.id) // propertyShape(p)
    _ <- addTriple(id, sh_property, node)
  } yield ()

  def closed(id: RDFNode, b: Boolean): RDFSaver[Unit] =
    addTriple(id, sh_closed, BooleanLiteral(b))

  def ignoredProperties(id: RDFNode, ignored: List[IRI]): RDFSaver[Unit] =
    if (!ignored.isEmpty) {
      for {
        nodeList <- saveToRDFList(ignored, (iri: IRI) => State.pure(iri))
        _ <- addTriple(id, sh_ignoredProperties, nodeList)
      } yield ()
    } else
      State.pure(())

  def propertyShape(t: PropertyShape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(t.id)
    _ <- addTriple(shapeNode, rdf_type, sh_PropertyShape)
    _ <- targets(shapeNode, t.targets)
    _ <- propertyShapes(shapeNode, t.propertyShapes)
    _ <- closed(shapeNode, t.closed)
    _ <- ignoredProperties(shapeNode, t.ignoredProperties)
    pathNode <- makePath(t.path)
    _ <- addTriple(shapeNode, sh_path, pathNode)
    _ <- saveList(t.components.toList, component(shapeNode))
  } yield (shapeNode)

  def nodeShape(n: NodeShape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(n.id)
    _ <- addTriple(shapeNode, rdf_type, sh_NodeShape)
    _ <- targets(shapeNode, n.targets)
    _ <- propertyShapes(shapeNode, n.propertyShapes)
    _ <- closed(shapeNode, n.closed)
    _ <- ignoredProperties(shapeNode, n.ignoredProperties)
    _ <- saveList(n.components, component(shapeNode))
  } yield shapeNode

  def makePath(path: SHACLPath): RDFSaver[RDFNode] = path match {
    case PredicatePath(iri) => State.pure(iri)
    case InversePath(p) => for {
      node <- createBNode
      pathNode <- makePath(p)
      _ <- addTriple(node, sh_inversePath, pathNode)
    } yield node
    case ZeroOrOnePath(p) => for {
      node <- createBNode
      pathNode <- makePath(p)
      _ <- addTriple(node, sh_zeroOrOnePath, pathNode)
    } yield node
    case ZeroOrMorePath(p) => for {
      node <- createBNode
      pathNode <- makePath(p)
      _ <- addTriple(node, sh_zeroOrMorePath, pathNode)
    } yield node
    case OneOrMorePath(p) => for {
      node <- createBNode
      pathNode <- makePath(p)
      _ <- addTriple(node, sh_oneOrMorePath, pathNode)
    } yield node
    /*    case SequencePath(ps) => for {
      list <- saveRDFList(ps, )
      pathNodes <- makePath(p)
      _ <- addTriple(node,sh_oneOrMorePath,pathNode)
    } yield node
    case AlternativePath(ps) => for {
      node <- createBNode
      pathNodes <- makePath(p)
      _ <- addTriple(node,sh_oneOrMorePath,pathNode)
    } yield node */
    case _ => throw new Exception(s"Not implemented path generation to RDF yet: $path")
  }

  def component(id: RDFNode)(c: Component): RDFSaver[Unit] = c match {
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

