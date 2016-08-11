package es.weso.shacl

import util._
import es.weso.rdf.RDFBuilder
import cats._, data._
import cats.syntax.all._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import es.weso.rdf._
import es.weso.rdf.triples._
import es.weso.rdf.nodes._
import cats.std.list._
import SHACLPrefixes._
import es.weso.rdf.PREFIXES._
import cats.std.list._
import es.weso.rdf.jena._


object Shacl2RDF {

  type RDFBuild = Fx.fx2[State[RDFAsJenaModel,?],Eval]
  type RDFSaver[A] = Eff[RDFBuild,A]

  def serialize(shacl:Schema, format: String): Try[String] = {
    val rdf: RDFAsJenaModel = toRDF(shacl,RDFAsJenaModel.empty)
    Success(rdf.serialize(format))
  }
  
  def toRDF(shacl: Schema, initial: RDFAsJenaModel): RDFAsJenaModel = {
    val result = schema(shacl).runState(initial).runEval.run
    result._2
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
    shapeNode <- makeId(shape.id)
    _ <- targets(shapeNode, shape.targets)
    _ <- constraints(shapeNode, shape.constraints)
  } yield shapeNode
  
  def makeId(v: Option[IRI]): RDFSaver[RDFNode] = v match {
    case None => for {
      bNode <- createBNode()
      _ <- addTriple(bNode,rdf_type,sh_Shape)
    } yield(bNode)
    case Some(iri) =>  
      addTriple(iri,rdf_type,sh_Shape) >> pure(iri)
  }
  
  def targets(id: RDFNode, ts: Seq[Target]): RDFSaver[Unit] = 
    saveList(ts.toList, target(id))
    
  def target(id: RDFNode)(t: Target): RDFSaver[Unit] = t match {
    case TargetNode(node) => addTriple(id,sh_targetNode,node)
  }
  
  def constraints(id: RDFNode, ts: Seq[Constraint]): RDFSaver[Unit] = 
    saveList(ts.toList, constraint(id))
    
  def constraint(id: RDFNode)(t: Constraint): RDFSaver[Unit] = t match {
    case PropertyConstraint(i,pred,cs) => for {
      node <- makeId(i)
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
                               case None => pure(())
                             })
    case Stem(s) => addTriple(id,sh_stem,StringLiteral(s))                             
    case UniqueLang(b) => addTriple(id,sh_uniqueLang,BooleanLiteral(b))                             
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
      nodeList <- saveToRDFList(ignoredPs,(iri: IRI) => pure(iri))
      _ <- addTriple(id,sh_ignoredProperties,nodeList)
    } yield ()
    case ShapeComponent(s) => for {
      nodeS <- shape(s)
      _ <- addTriple(id,sh_shape,nodeS)
    } yield ()
    case HasValue(v) => addTriple(id,sh_hasValue,v.rdfNode)
    case In(vs) => for {
      nodeLs <- saveToRDFList(vs, (v: Value) => pure(v.rdfNode))
      _ <- addTriple(id,sh_in,nodeLs)
    } yield ()
    
  }
  
  
  def saveList[A](ls: List[A], f: A => RDFSaver[Unit]): RDFSaver[Unit] = { 
    ls.map(f(_)).sequence.map(_ => ())
  }
  
  def saveToRDFList[A](ls: List[A], f: A => RDFSaver[RDFNode]): RDFSaver[RDFNode] = ls match {
    case Nil => pure(rdf_nil)
    case x :: xs => for {
      nodeX <- f(x)
      bNode <- createBNode
      _ <- addTriple(bNode,rdf_first,nodeX)
      rest <- saveToRDFList(xs,f)
      _ <- addTriple(bNode,rdf_rest,rest)
    } yield bNode
  }
  
  def addTriple(s: RDFNode, p: IRI, o: RDFNode): RDFSaver[Unit] = 
    modify[RDFBuild,RDFAsJenaModel](_.addTriple(RDFTriple(s,p,o)))
  
  def addPrefix(alias: String, value: String): RDFSaver[Unit] = 
    modify[RDFBuild,RDFAsJenaModel](_.addPrefix(alias,value))

    def createBNode(): RDFSaver[RDFNode] = for {
    rdf <- get[RDFBuild,RDFAsJenaModel]
    val (bNode,newRdf) = rdf.createBNode
    _ <- put[RDFBuild,RDFAsJenaModel](newRdf)
  } yield bNode
    
  
}