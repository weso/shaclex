package es.weso.shex.shexR
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.parser.RDFParser
import es.weso.shex._
import es.weso.rdf.PREFIXES._
import es.weso.shex.shexR.PREFIXES._
import cats._
import cats.implicits._
import es.weso.rdf.nodes.{BNodeId, IRI, RDFNode}

import scala.util.{Failure, Success, Try}

object RDF2ShEx extends RDFParser with LazyLogging {

  def getSchemas (rdf: RDFReader): Try[List[Schema]] = {
    val schemaNodes = rdf.triplesWithPredicateObject(rdf_type, sx_Schema).map(_.subj).toList
    schemaNodes.map(schema(_,rdf)).sequence
  }

  def schema: RDFParser[Schema] = (n,rdf) => for {
    _ <- checkType(sx_Schema)(n,rdf)
    startActions <- opt(sx_startActs, semActList1Plus)(n,rdf)
    start <- opt(sx_start, shapeExpr)(n,rdf)
    shapePairs <- starWithNodes(sx_shapes, shapeExpr)(n,rdf)
    shapeMap <- cnvShapePairs(shapePairs)
  } yield {
    val shapes = if (shapeMap.isEmpty) None
                 else Some(shapeMap)
    Schema(Some(rdf.getPrefixMap()),None,startActions,start,shapes)
  }

  def cnvShapePairs(ps: List[(RDFNode,ShapeExpr)]): Try[Map[ShapeLabel,ShapeExpr]] = {
    ps.map(cnvShapePair).sequence.map(_.toMap)
  }

  def cnvShapePair(p: (RDFNode,ShapeExpr)): Try[(ShapeLabel,ShapeExpr)] =
    toLabel(p._1).map(l => (l,p._2))

  def toLabel(node: RDFNode): Try[ShapeLabel] = node match {
    case i: IRI => Success(IRILabel(i))
    case b: BNodeId => Success(BNodeLabel(b))
    case _ => fail(s"node $node must be an IRI or a BNode in order to be a ShapeLabel")
  }


  def opt[A](pred: IRI, parser: RDFParser[A]): RDFParser[Option[A]] = (n,rdf) => {
    objectFromPredicate(pred)(n,rdf) match {
      case Success(v) => parser(v,rdf).map(Some(_))
      case Failure(_) => Success(None)
    }
  }

  def starWithNodes[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[(RDFNode, A)]] = (n,rdf) => {
    for {
      os <- objectsFromPredicate(pred)(n,rdf).map(_.toList)
      vs : List[(RDFNode,A)] <- os.map(node => parser(node,rdf).map(v => (node,v))).sequence
    } yield vs
  }


  def star[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] = (n,rdf) =>
    for {
     os <- objectsFromPredicate(pred)(n,rdf).map(_.toList)
     vs : List[A] <- os.map(node => parser(node,rdf)).sequence
  } yield vs

  def shapeExpr: RDFParser[ShapeExpr] = ???

  def semActList1Plus: RDFParser[List[SemAct]] = (n,rdf) => {
    Success(List())
  }

  def checkType(expected: RDFNode): RDFParser[Boolean] = (n,rdf) => for {
    obtained <- objectFromPredicate(rdf_type)(n,rdf)
    v <- if (obtained == expected) Success(true)
         else
          fail(s"Type of node $n must be $expected but obtained $obtained")
  } yield v

  def fail[A](str: String): Try[A] =
    Failure(throw new Exception(str))


}