package es.weso.schemaInfer
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.schema.{Schema, ShExSchema, ShaclexSchema}
import es.weso.schema.Schemas._
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._

object SchemaInfer {

  // Namespace for annotations
  val sx = IRI("http://weso.es/ns/shex/")

  case class InferOptions(
   inferTypePlainNode: Boolean,
   addXSDAlias: Boolean,
   addRDFAlias: Boolean,
   addSxAlias: Boolean
  )
  object InferOptions {
    val defaultOptions =
      InferOptions(inferTypePlainNode = true,
        addXSDAlias = true,
        addRDFAlias = true,
        addSxAlias = true
      )
  }

  def infer(rdf: RDFReader,
            selector: NodeSelector,
            engine: String,
            shapeLabel: IRI,
            options: InferOptions = InferOptions.defaultOptions
           ): Either[String, Schema] = for {
    nodes <- selector.select(rdf)
    neighbours <- getNeighbourhoods(nodes, rdf)
    schema <- mkSchema(
      neighbours.mapValues(collapse(options)),
      engine, shapeLabel, rdf.getPrefixMap(),
      options
    )
  } yield schema

  type NeighMap = Map[IRI, Set[RDFNode]]
  type EitherNeighMap = Either[String,NeighMap]

  private def getNeighbourhoods(nodes: Set[RDFNode],
                        rdf: RDFReader
                       ): EitherNeighMap = {
    val zero: EitherNeighMap = Right(Map())
    def combine(rest: EitherNeighMap,
                node: RDFNode
               ): EitherNeighMap = for {
     ts <- rdf.triplesWithSubject(node)
    } yield ts.map(
       triple => (triple.pred, triple.obj)
     ).groupBy(_._1).map { case (k,v) => (k, v.map(_._2))}

    nodes.foldLeft(zero)(combine)
  }

  private def collapse(options: InferOptions)
                      (nodes: Set[RDFNode]): InferredValue = {
   if (nodes.isEmpty) InferredValue(InferredNone,0)
   else {
     def zero = inferNode(nodes.head, options)
     def combine(rest: InferredNodeConstraint, node: RDFNode): InferredNodeConstraint = {
       collapsePair(rest,node)
     }
     val c = nodes.tail.foldLeft(zero)(combine)
     InferredValue(c,nodes.size)
   }
  }

  private def inferNode(node: RDFNode, options: InferOptions): InferredNodeConstraint =
    if (options.inferTypePlainNode) {
      node match {
        case iri: IRI => InferredIRI
        case l: StringLiteral => InferredString
        case l: LangLiteral => InferredLang(l.lang)
        case l: DatatypeLiteral => InferredDatatype(l.dataType)
        case l: Literal => InferredDatatype(l.dataType)
        case b: BNode => InferredBlankNode
      }
    } else PlainNode(node)

  private def collapsePair(nk: InferredNodeConstraint,
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
    case InferredLiteral =>
      if (node.isLiteral) InferredLiteral
      else InferredNone
    case InferredDatatype(dt) => node match {
      case l: Literal =>
        if (dt == l.dataType) InferredDatatype(dt)
        else InferredLiteral
      case _ => InferredNone
    }
    case InferredLang(lang) => node match {
      case l: LangLiteral =>
        if (l.lang == lang) InferredLang(lang)
        else InferredLangString
      case l: Literal => InferredLiteral
      case _ => InferredNone
    }
    case InferredLangString => node match {
      case l: LangLiteral => InferredLangString
      case l: Literal => InferredLiteral
      case _ => InferredNone
    }
    case InferredNone => InferredNone

  }

  private def collectKind(n1: RDFNode, n2: RDFNode): InferredNodeConstraint =
    (n1,n2) match {
      case (i1:IRI, i2: IRI) => InferredIRI
      case (b1: BNode, b2: BNode) => InferredBlankNode
      case (l1: LangLiteral, l2: LangLiteral) =>
        if (l1.lang == l2.lang) InferredLang(l1.lang)
        else InferredLangString
      case (l1: Literal, l2: Literal) => (l1.dataType,l2.dataType) match {
        case (`xsd_string`, `xsd_string`) => InferredString
        case (dt1,dt2) => if (dt1 == dt2) InferredDatatype(dt1)
        else InferredLiteral
      }
      case (_,_) => InferredNone
    }

  private def hasXSD(c: InferredValue): Boolean = c.constraint match {
    case InferredString => true
    case InferredDatatype(dt) =>
      if (dt.getLexicalForm.startsWith(xsd.str)) true
      else false
    case _ => false
  }
  private def hasRDF(c: InferredValue): Boolean = c.constraint match {
    case InferredLangString => true
    case _ => false
  }
  private def hasStar(c: InferredValue): Boolean =
    c.number > 1

  private def containsDecl(m: Map[IRI,InferredValue],
                           check: InferredValue => Boolean,
                           name: String): Boolean = {
    !m.values.filter(check).isEmpty
  }
  private def addPrefix(alias: String,
                        iri: IRI,
                        pm: PrefixMap,
                        options: InferOptions,
                        checkOptions: InferOptions => Boolean,
                        m: Map[IRI,InferredValue],
                        check: InferredValue => Boolean): PrefixMap = {
    if (containsDecl(m, check, alias)) {
      if (checkOptions(options) && !pm.contains(alias)) pm.addPrefix(alias,iri)
      else pm
    }
    else pm
  }

  private def mkSchema(m: Map[IRI,InferredValue],
                 engine: String,
                 label: IRI,
                 pm: PrefixMap,
                 inferOptions: InferOptions
                ): Either[String,Schema] =
   lookupSchema(engine) match {
    case Right(_: ShExSchema) => Right(ShExSchema(mkShExSchema(m,label,pm,inferOptions)))
    case Right(_: ShaclexSchema) => Left(s"Not implemented yet")
    case Left(e) => Left(s"Not found engine $engine, Error: $e")
    case l => Left(s"Not found schema $engine. Found: $l")
  }

  private def strPredicate(tc: TripleConstraint): String = tc.predicate.getLexicalForm

  private def mkShExSchema(m: Map[IRI,InferredValue],
                   label: IRI,
                   pm: PrefixMap,
                   inferOptions: InferOptions
                  ): es.weso.shex.Schema = {
    val shapeLabel = IRILabel(label)
    val es : List[TripleExpr] = m.map { case (iri, c) => {
      val totalNumber = sx + "totalNumber"
      val as: Option[List[Annotation]] = if (c.number > 1) Some(List(
        Annotation(totalNumber, ObjectValue.intValue(c.number)))
      ) else None
      TripleConstraint(None,None,None,iri,
        Some(mkShExConstraint(c.constraint, inferOptions)),
        Some(mkMinCardinality(c.number)),
        Some(mkMaxCardinality(c.number)), None, None, as
      )
    }
    }.toList.sortBy(strPredicate)
    val expr: Option[TripleExpr] = es.length match {
      case 0 => None
      case 1 => Some(es.head)
      case _ => Some(EachOf(None, es, None, None, None, None))
    }
    val shape = Shape(Some(shapeLabel),None,None,None,expr,None,None,None)

    val pm1 = addPrefix("xsd",xsd,pm,inferOptions, _.addXSDAlias,m,hasXSD)
    val pm2 = addPrefix("rdf",rdf,pm1,inferOptions, _.addRDFAlias,m,hasRDF)
    val pm3 = addPrefix("sx",sx,pm2,inferOptions, _.addSxAlias,m,hasStar)
    Schema(IRI(""),Some(pm3), None, None, None, Some(List(shape)), None, List())
  }

  private case class InferredValue(constraint: InferredNodeConstraint, number: Int)

  private sealed abstract trait InferredNodeConstraint
  private case class PlainNode(node: RDFNode) extends InferredNodeConstraint
  private case object InferredIRI extends InferredNodeConstraint
  private case object InferredBlankNode extends InferredNodeConstraint
  private case object InferredLiteral extends InferredNodeConstraint
  private case object InferredString extends InferredNodeConstraint
  private case class InferredLang(lang: Lang) extends InferredNodeConstraint
  private case object InferredLangString extends InferredNodeConstraint
  private case class InferredDatatype(dt: IRI) extends InferredNodeConstraint
  private case object InferredNone extends InferredNodeConstraint

  private def mkShExConstraint(c: InferredNodeConstraint, options: InferOptions): NodeConstraint = c match {

    case PlainNode(node) => node match {
      case iri: IRI =>
        NodeConstraint.valueSet(List(IRIValue(iri)),List())
      case l: StringLiteral =>
        NodeConstraint.valueSet(List(StringValue(l.getLexicalForm)), List())
      case l: LangLiteral =>
        NodeConstraint.valueSet(List(LangString(l.getLexicalForm, l.lang)), List())
      case l: DatatypeLiteral =>
        NodeConstraint.valueSet(List(DatatypeString(l.getLexicalForm, l.dataType)), List())
      case l: Literal =>
        NodeConstraint.valueSet(List(DatatypeString(l.getLexicalForm, l.dataType)), List())
      case b: BNode =>
        NodeConstraint.nodeKind(BNodeKind, List())
    }
    case InferredIRI => NodeConstraint.nodeKind(IRIKind, List())
    case InferredBlankNode => NodeConstraint.nodeKind(BNodeKind, List())
    case InferredLiteral => NodeConstraint.nodeKind(LiteralKind, List())
    case InferredString => NodeConstraint.datatype(`xsd_string`, List())
    case InferredLang(lang) => NodeConstraint.valueSet(List(LanguageStem(lang)), List())
    case InferredLangString => NodeConstraint.datatype(`rdf_langString`, List())
    case InferredDatatype(dt) => NodeConstraint.datatype(dt, List())
    case InferredNone => NodeConstraint.empty
  }

  private def mkMinCardinality(n: Integer): Integer =
    if (n == 1) 1
    else 0

  private def mkMaxCardinality(n: Integer): Max =
    if (n == 1) IntMax(1)
    else Star

}