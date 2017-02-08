package es.weso.shacl.converter
import scala.util._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES.{sh => _, _}
import es.weso.rdf.jena._
import es.weso.shacl._

class Shacl2RDF extends RDFSaver {

  def serialize(shacl:Schema, format: String): Try[String] = {
    val rdf: RDFAsJenaModel = toRDF(shacl,RDFAsJenaModel.empty)
    Success(rdf.serialize(format))
  }

  def toRDF(shacl: Schema, initial: RDFAsJenaModel): RDFAsJenaModel = {
    val result = schema(shacl).run(initial)
    result.value._1
  }

  def schema(shacl: Schema): RDFSaver[Unit] = {
    val rs = shacl.shapes.toList.map(shape(_))
    for {
      _ <- addPrefix("sh",sh.str)
      _ <- addPrefix("xsd",xsd.str)
      _ <- addPrefix("rdf",rdf.str)
      _ <- addPrefix("rdfs",rdfs.str)
      _ <- rs.sequence
    } yield ()
  }

  def shape(shape: Shape): RDFSaver[RDFNode] = for {
    shapeNode <- makeShapeId(shape.id)
    _ <- targets(shapeNode, shape.targets)
    _ <- constraints(shapeNode, shape.constraints)
    _ <- closed(shapeNode, shape.closed)
    _ <- ignoredProperties(shapeNode, shape.ignoredProperties)
  } yield shapeNode

  def makeShapeId(v: Option[IRI]): RDFSaver[RDFNode] = for {
    node <- makeId(v)
    _ <- addTriple(node,rdf_type,sh_Shape)
  } yield(node)

  def targets(id: RDFNode, ts: Seq[Target]): RDFSaver[Unit] =
    saveList(ts.toList, target(id))

  def target(id: RDFNode)(t: Target): RDFSaver[Unit] = t match {
    case TargetNode(node) => addTriple(id,sh_targetNode,node)
    case TargetClass(node) => addTriple(id,sh_targetClass,node)
    case TargetSubjectsOf(node) => addTriple(id,sh_targetSubjectsOf,node)
    case TargetObjectsOf(node) => addTriple(id,sh_targetObjectsOf,node)
  }

  def constraints(id: RDFNode, ts: Seq[Constraint]): RDFSaver[Unit] =
    saveList(ts.toList, constraint(id))

  def closed(id: RDFNode, b: Boolean): RDFSaver[Unit] =
    addTriple(id, sh_closed, BooleanLiteral(b))

  def ignoredProperties(id: RDFNode, ignored: List[IRI]): RDFSaver[Unit] =
    if (!ignored.isEmpty) {
      for {
        nodeList <- saveToRDFList(ignored,(iri: IRI) => State.pure(iri))
        _ <- addTriple(id,sh_ignoredProperties,nodeList)
      } yield ()
    } else
      State.pure(())

  def constraint(id: RDFNode)(t: Constraint): RDFSaver[Unit] = t match {
    case PropertyConstraint(i,pred,cs) => for {
      node <- makeShapeId(i)
      _ <- addTriple(id, sh_property, node)
      _ <- addTriple(node,sh_predicate,pred)
      _ <- saveList(cs.toList, component(node))
    } yield ()
    case pc: PathPropertyConstraint => ???
    case NodeConstraint(cs) => saveList(cs, component(id))
  }

  def component(id: RDFNode)(c: Component): RDFSaver[Unit] = c match {
    case ClassComponent(v) => addTriple(id, sh_class, v)
    case Datatype(iri) => addTriple(id, sh_datatype, iri)
    case NodeKind(value) => addTriple(id, sh_nodeKind, value.id)
    case MinCount(n) => addTriple(id,sh_minCount,IntegerLiteral(n))
    case MaxCount(n) => addTriple(id,sh_maxCount,IntegerLiteral(n))
    case MinExclusive(v) => addTriple(id,sh_minExclusive,v)
    case MinInclusive(v) => addTriple(id,sh_minInclusive,v)
    case MaxExclusive(v) => addTriple(id,sh_maxExclusive,v)
    case MaxInclusive(v) => addTriple(id,sh_maxInclusive,v)
    case MinLength(n) => addTriple(id,sh_minLength,IntegerLiteral(n))
    case MaxLength(n) => addTriple(id,sh_maxLength,IntegerLiteral(n))
    case Pattern(p,flags) => addTriple(id,sh_pattern,StringLiteral(p)) >>
                             ( flags match {
                               case Some(f) => addTriple(id,sh_flags,StringLiteral(f))
                               case None => State.pure(())
                             })
    case Stem(s) => addTriple(id,sh_stem,StringLiteral(s))
    case UniqueLang(b) => addTriple(id,sh_uniqueLang,BooleanLiteral(b))
    case Equals(p) => addTriple(id,sh_equals,p)
    case Disjoint(p) => addTriple(id,sh_disjoint,p)
    case LessThan(p) => addTriple(id,sh_lessThan,p)
    case LessThanOrEquals(p) => addTriple(id,sh_lessThanOrEquals,p)
    case And(shapes) => for {
      ls <- saveToRDFList(shapes,shape)
      _ <- addTriple(id,sh_and,ls)
    } yield ()
    case Or(shapes) => for {
      ls <- saveToRDFList(shapes,shape)
      _ <- addTriple(id,sh_or,ls)
    } yield ()
    case Not(s) => for {
      nodeS <- shape(s)
      _ <- addTriple(id,sh_not,nodeS)
    } yield ()
    case Closed(b,ignoredPs) => for {
      _ <- addTriple(id,sh_closed,BooleanLiteral(b))
      nodeList <- saveToRDFList(ignoredPs,(iri: IRI) => State.pure(iri))
      _ <- addTriple(id,sh_ignoredProperties,nodeList)
    } yield ()
    case ShapeComponent(s) => for {
      nodeS <- shape(s)
      _ <- addTriple(id,sh_shape,nodeS)
    } yield ()
    case HasValue(v) => addTriple(id,sh_hasValue,v.rdfNode)
    case In(vs) => for {
      nodeLs <- saveToRDFList(vs, (v: Value) => State.pure(v.rdfNode))
      _ <- addTriple(id,sh_in,nodeLs)
    } yield ()

  }

}

