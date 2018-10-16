package es.weso.schemaInfer
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.schema.{Schema, Schemas, ShExSchema}
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._

object SchemaInfer {

  def infer(rdf: RDFReader,
            selector: NodeSelector,
            engine: String,
            shapeLabel: IRI
           ): Either[String, Schema] = for {
    nodes <- selector.select(rdf)
    schema <- mkSchema(
      getNeighbourhoods(nodes, rdf).mapValues(collapse),
      engine, shapeLabel, rdf.getPrefixMap()
    )
  } yield schema

  def getNeighbourhoods(nodes: Set[RDFNode],
                        rdf: RDFReader
                       ): Map[IRI, Set[RDFNode]] = {
    val zero: Map[IRI, Set[RDFNode]] = Map()
    def combine(rest: Map[IRI, Set[RDFNode]],
                node: RDFNode
               ): Map[IRI, Set[RDFNode]] = {
     rdf.triplesWithSubject(node).map(
       triple => (triple.pred, triple.obj)
     ).groupBy(_._1).map { case (k,v) => (k, v.map(_._2))}
    }
    nodes.foldLeft(zero)(combine)
  }

  def collapse(nodes: Set[RDFNode]): InferredNodeConstraint = {
   if (nodes.isEmpty) InferredNone
   else {
     def zero: InferredNodeConstraint = PlainNode(nodes.head)
     def combine(rest: InferredNodeConstraint, node: RDFNode): InferredNodeConstraint = {
       collapsePair(rest,node)
     }
     nodes.tail.foldLeft(zero)(combine)
   }
  }

  def collapsePair(nk: InferredNodeConstraint,
                   node: RDFNode
                  ): InferredNodeConstraint = nk match {
    case PlainNode(n) =>
      if (node == n) PlainNode(n)
      else collectKind(n,node)
    case InferredIRI =>
      if (node.isIRI) InferredIRI
      else InferredNone
    case InferredBlankNode =>
      if (node.isBNode) InferredBlankNode
      else InferredNone
    case InferredString => node match {
      case s : StringLiteral => InferredString
      case DatatypeLiteral(_,`xsd_string`) => InferredString
      case DatatypeLiteral(_,_) => InferredLiteral
      case _ => InferredNone
    }
    case InferredDatatype(dt) => node match {
      case DatatypeLiteral(_,dt1) =>
        if (dt == dt1) InferredDatatype(dt)
        else InferredLiteral
      case _ => InferredNone
    }
    case InferredNone => InferredNone

  }

  def collectKind(n1: RDFNode, n2: RDFNode): InferredNodeConstraint =
    (n1,n2) match {
      case (i1:IRI, i2: IRI) => InferredIRI
      case (b1: BNode, b2: BNode) => InferredBlankNode
      case (l1: Literal, l2: Literal) => (l1.dataType,l2.dataType) match {
        case (`xsd_string`, `xsd_string`) => InferredString
        case (dt1,dt2) => if (dt1 == dt2) InferredDatatype(dt1)
        else InferredLiteral
      }
      case (_,_) => InferredNone
    }

  def mkSchema(m: Map[IRI,InferredNodeConstraint],
                 engine: String,
                 label: IRI,
                 pm: PrefixMap
                ): Either[String,Schema] =
   Schemas.lookupSchema(engine) match {
    case shEx => Right(ShExSchema(mkShExSchema(m,label,pm)))
    case shaclex => Left(s"Not implemented yet")
    case _ => Left(s"Not found schema $engine")
  }

  def mkShExSchema(m: Map[IRI,InferredNodeConstraint],
                   label: IRI,
                   pm: PrefixMap): es.weso.shex.Schema = {
    val shapeLabel = IRILabel(label)
    val es : List[TripleExpr] = m.map { case (iri, c) =>
      TripleConstraint(None,None,None,iri,
        Some(mkShExConstraint(c)),
        Some(1),
        Some(mkMaxCardinality(c)), None, None, None
      )
    }.toList
    val expr: Option[TripleExpr] = es.length match {
      case 0 => None
      case 1 => Some(es.head)
      case _ => Some(EachOf(None, es, None, None, None, None))
    }
    val shape = Shape(Some(shapeLabel),None,None,None,expr,None,None,None)
    Schema(Some(pm), None, None, None, Some(List(shape)), None, List())
  }

  sealed abstract trait InferredNodeConstraint
  case class PlainNode(node: RDFNode) extends InferredNodeConstraint
  case object InferredIRI extends InferredNodeConstraint
  case object InferredBlankNode extends InferredNodeConstraint
  case object InferredLiteral extends InferredNodeConstraint
  case object InferredString extends InferredNodeConstraint
  case class InferredDatatype(dt: IRI) extends InferredNodeConstraint
  case object InferredNone extends InferredNodeConstraint


  def mkShExConstraint(c: InferredNodeConstraint): NodeConstraint = c match {

    case PlainNode(node) => node match {
      case iri: IRI => NodeConstraint.valueSet(List(IRIValue(iri)),List())
      case l: StringLiteral => NodeConstraint.valueSet(List(StringValue(l.getLexicalForm)), List())
      case l: LangLiteral => NodeConstraint.valueSet(List(LangString(l.getLexicalForm, l.lang)), List())
      case l: Literal => NodeConstraint.valueSet(List(DatatypeString(l.getLexicalForm, l.dataType)), List())
      case b: BNode => NodeConstraint.nodeKind(BNodeKind, List())
    }
    case InferredIRI => NodeConstraint.nodeKind(IRIKind, List())
    case InferredBlankNode => NodeConstraint.nodeKind(BNodeKind, List())
    case InferredLiteral => NodeConstraint.nodeKind(LiteralKind, List())
    case InferredString => NodeConstraint.datatype(`xsd_string`, List())
    case InferredDatatype(dt) => NodeConstraint.datatype(dt, List())
    case InferredNone => NodeConstraint.empty
  }

  def mkMaxCardinality(c: InferredNodeConstraint): Max = c match {
    case PlainNode(_) => IntMax(1)
    case _ => Star
  }

}