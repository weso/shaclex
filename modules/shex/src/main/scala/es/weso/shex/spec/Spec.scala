package es.weso.shex.spec

import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, Literal, RDFNode}
import es.weso.shapeMaps.{FixedShapeMap, ShapeMapLabel}
import es.weso.shex._
import es.weso.shex.btValidator.ShExErr
import es.weso.typing.Typing
import Check._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.triples.RDFTriple
import es.weso.shex.spec.Spec.neighs
import es.weso.utils.SetUtils

object Spec extends LazyLogging {

  type ShapeTyping = Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]]

  def satisfies(n: RDFNode, se: ShapeExpr, rdf: RDFReader, m: FixedShapeMap): Check[Boolean] = se match {
    case nc: NodeConstraint => satisfies2(n, nc)
    case ShapeAnd(_,ses) => satisfyAll(ses.map(satisfies(n,_,rdf,m)))
    case ShapeOr(_,ses) => satisfySome(ses.map(satisfies(n,_,rdf,m)))
    case ShapeNot(_,se) => satisfyNot(satisfies(n,se,rdf,m))
    case ShapeRef(lbl) => satisfyShapeRef(n,lbl,rdf,m)
    case ShapeExternal(id) => unimplemented(s"ShapeExpternal($id)")
    case s: Shape => satisfyShape(n,s,rdf,m)
  }

  def satisfyShapeRef(n: RDFNode, lbl: ShapeLabel, rdf: RDFReader, m: FixedShapeMap): Check[Boolean] =
   for {
    se <- getShape(lbl)
    v <- satisfies(n,se,rdf,m)
   } yield v

  def satisfyShape(n: RDFNode, shape: Shape, rdf: RDFReader, m: FixedShapeMap): Check[Boolean] =
    shape.expression match {
      case None => ???
      case Some(te) => {
        for {
          neighs <- neighs(n,rdf)
          b <- satisfyFirst(SetUtils.pSet(neighs), satisfyMatches(te,rdf,m))
        } yield b
      }
    }

  def satisfyMatches(
                      te: TripleExpr,
                      rdf: RDFReader,
                      m: FixedShapeMap
                    )(pair: (Set[RDFTriple], Set[RDFTriple])): Check[Boolean] = {
    val (matched,remainder) = pair
    matches(matched,te,m)
    // TODO: rest of conditions
  }

  def matches(matched: Set[RDFTriple], te: TripleExpr, m: FixedShapeMap): Check[Boolean] = ???

  def neighs(n: RDFNode, rdf: RDFReader): Check[Set[RDFTriple]] = {
    val outArcs = rdf.triplesWithSubject(n)
    val inArcs = rdf.triplesWithObject(n)
    pure(outArcs ++ inArcs)
  }


  def satisfies2(n: RDFNode, nc: NodeConstraint): Check[Boolean] = nodeSatisfies(n,nc)

  def nodeSatisfies(n: RDFNode, nc: NodeConstraint): Check[Boolean] = for {
    nk <- optSatisfy(nc.nodeKind, satisfiesNodeKind(n))
    dt <- optSatisfy(nc.datatype, satisfiesDatatype(n))
  } yield List(nk,dt).forall(_ == true)

  def satisfiesNodeKind(n: RDFNode)(nk: NodeKind): Check[Boolean] = nk match {
    case IRIKind => pure(n.isIRI)
    case BNodeKind => pure(n.isBNode)
    case LiteralKind => pure(n.isLiteral)
    case NonLiteralKind => pure(n.isNonLiteral)
  }

  def satisfiesDatatype(n: RDFNode)(dt: IRI): Check[Boolean] = n match {
    case l: Literal => pure(l.dataType == dt)
    case _ => pure(false)
  }

  def getShape(lbl: ShapeLabel): Check[ShapeExpr] = for {
    schema <- getSchema
    shape <- schema.getShape(lbl) match {
      case None => err(s"Not found $lbl in schema")
      case Some(se) => pure(se)
    }
  } yield shape


}