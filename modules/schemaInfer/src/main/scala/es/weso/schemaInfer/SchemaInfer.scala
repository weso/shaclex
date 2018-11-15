package es.weso.schemaInfer
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.schema.{Schema, ShExSchema, ShaclexSchema}
import es.weso.schema.Schemas._
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple

object SchemaInfer {

  // Namespace for annotations
  val sx = IRI("http://weso.es/ns/shex/")
  val wikidataId = "Endpoint(https://query.wikidata.org/sparql)"
  val wikibase = IRI("http://wikiba.se/ontology#")
  val schemaIri = IRI("http://schema.org/")
  val propIri   = IRI("http://www.wikidata.org/prop/")
  val `wikibase:directClaim` = wikibase + "directClaim"
  val prov = IRI("http://www.w3.org/ns/prov#")
  val `prov:wasDerivedFrom` = prov + "wasDerivedFrom"

  type NeighMap = Map[IRI, Set[RDFNode]]
  type EitherNeighMap = Either[String,NeighMap]
  type InferredShape = Map[IRI,InferredValue]
  type InferredSchema = Map[IRI, InferredShape]

  type ES[A] = Either[String,A]
  val totalNumber = sx + "totalNumber"

  case class InferOptions(
   inferTypePlainNode: Boolean,
   addXSDAlias: Boolean,
   addRDFAlias: Boolean,
   addSxAlias: Boolean,
   addRDFSAlias: Boolean,
   addWDPropAlias: Boolean,
   addSchemaAlias: Boolean,
   addLabelLang: Option[Lang],
   followOnPreds: List[(IRI, IRI)]
  )
  object InferOptions {
    val defaultOptions =
      InferOptions(
        inferTypePlainNode = true,
        addXSDAlias = true,
        addRDFAlias = true,
        addSxAlias = true,
        addRDFSAlias = true,
        addWDPropAlias = true,
        addSchemaAlias = true,
        addLabelLang = Some(Lang("en")),
        followOnPreds = List((`prov:wasDerivedFrom`, IRI("references")))
      )
  }

  def inferSchema(rdf: RDFReader,
                  selector: NodeSelector,
                  engine: String,
                  shapeLabel: IRI,
                  options: InferOptions
                 ): Either[String,Schema] = for {
    nodes <- selector.select(rdf)
    neighMap <- getNeighbourhoods(nodes, rdf)
    inferredSchema <- inferShapes(rdf,neighMap,engine,shapeLabel,options)
    schema <- mkSchema(
      inferredSchema,
      engine, shapeLabel,
      rdf,
      options
    )
  } yield schema

  def inferShapes(rdf: RDFReader,
                  neighMap: NeighMap,
                  engine: String,
                  shapeLabel: IRI,
                  options: InferOptions = InferOptions.defaultOptions
           ): Either[String, InferredSchema] =
    Right(Map(shapeLabel -> neighMap.mapValues(collapse(options))))

//  def mkSchema(m: Map[IRI,InferredSchema], engine: String): Either[String,Schema] = ???


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

  private def hasWDProp(c: InferredValue) =  true
  private def hasSchema(c: InferredValue) =  true


  private def hasRDF(c: InferredValue): Boolean = c.constraint match {
    case InferredLangString => true
    case _ => false
  }
  private def hasStar(c: InferredValue): Boolean =
    c.number > 1

  private def containsDecl(m: Map[IRI,InferredShape],
                           check: InferredValue => Boolean,
                           name: String): Boolean = {
   // TODO: repair this one:
    //   !m.values.filter(check).isEmpty
    true
  }

  private def addPrefix(alias: String,
                        iri: IRI,
                        options: InferOptions,
                        checkOptions: InferOptions => Boolean,
                        m: Map[IRI,InferredShape],
                        check: InferredValue => Boolean)(pm: PrefixMap): PrefixMap = {
    if (containsDecl(m, check, alias)) {
      if (checkOptions(options) && !pm.contains(alias)) pm.addPrefix(alias,iri)
      else pm
    }
    else pm
  }

  private def mkSchema(m: InferredSchema,
                 engine: String,
                 label: IRI,
                 rdf: RDFReader,
                 inferOptions: InferOptions
                ): Either[String,Schema] =
   lookupSchema(engine) match {
    case Right(_: ShExSchema) => mkShExSchema(m, label, rdf, inferOptions)
    case Right(_: ShaclexSchema) => Left(s"Not implemented yet")
    case Left(e) => Left(s"Not found engine $engine, Error: $e")
    case l => Left(s"Not found schema $engine. Found: $l")
  }

//   private def strPredicate(tc: TripleConstraint): String = tc.predicate.getLexicalForm
  private def getLabel(iri: IRI,
                       rdf: RDFReader,
                       maybeLang: Option[Lang]
                      ): Either[String, Option[Literal]] = for {
   labels <-
     if (rdf.id == wikidataId) for {
       sourceTriples <- rdf.triplesWithPredicateObject(`wikibase:directClaim`, iri)
       sourceIRIs = sourceTriples.map(_.subj)
//       _ <- { println(s"sourceIRIs for $iri: $sourceIRIs"); Right(()) }
       labels <- sourceIRIs.map(rdf.triplesWithSubjectPredicate(_,rdfs_label)).toList.sequence[ES,Set[RDFTriple]]
//       _ <- { println(s"All labels for $iri: $labels"); Right(()) }
     } yield labels.flatten.map(_.obj)
     else {
      rdf.triplesWithSubjectPredicate(iri, rdfs_label).map(_.obj)
     }
  } yield {
    val okLabels = labels.collect {
      case l: Literal if hasLang(l,maybeLang) => l
    }
//    println(s"OKLabels for $iri: $okLabels")
    okLabels.headOption
  }

  private def hasLang(s: Literal, maybeLang: Option[Lang]): Boolean = {
    val b = maybeLang match {
      case None       => false
      case Some(lang) => s match {
        case l: LangLiteral => lang.matchLanguage(l.lang)
        case other => {
          // println(s"Other type for literal: ${other.getClass.getCanonicalName}")
          false
        }
      }
    }
//    println(s"Checking hasLang($s,$maybeLang): $b")
    b
  }

  private def mkTripleExpr(iri: IRI,
                           c: InferredValue,
                           rdf: RDFReader,
                           inferOptions: InferOptions
                          ): Either[String, TripleExpr] = for {
   label <- getLabel(iri, rdf, inferOptions.addLabelLang)
    // _ <- { println(s"Selected label for $iri: $label") ; Right(()) }
  } yield {
    val labelAnnotation: Option[Annotation] =
      label.map(lbl => Annotation(rdfs_label, ObjectValue.literalValue(lbl)))
    val totalNumberAnnotation: Option[Annotation] =
      if (c.number > 1) Some(Annotation(totalNumber, ObjectValue.intValue(c.number))) else None
    val as = {
      val ls = List(labelAnnotation, totalNumberAnnotation).collect{ case Some(a) => a }
      if (ls.isEmpty) None
      else Some(ls)
    }

    TripleConstraint(None,
                     None,
                     None,
                     iri,
                     Some(mkShExConstraint(c.constraint, inferOptions)),
                     Some(mkMinCardinality(c.number)),
                     Some(mkMaxCardinality(c.number)),
                     None,
                     None,
                     as)
  }

  private def mkShExShape(m: InferredShape,
                          label: IRI,
                          rdfReader: RDFReader,
                          inferOptions: InferOptions
                         ): Either[String, ShapeExpr]  = for {
    es <- m.map {
      case (iri, c) => mkTripleExpr(iri,c,rdfReader,inferOptions)
    }.toList.sequence[ES,TripleExpr]
  } yield {
    val shapeLabel = IRILabel(label)
    val expr: Option[TripleExpr] = es.length match {
      case 0 => None
      case 1 => Some(es.head)
      case _ => Some(EachOf(None, es, None, None, None, None))
    }
    Shape(Some(shapeLabel),None,None,None,expr,None,None,None)
  }

  private def mkShExSchema(m: InferredSchema,
                   label: IRI,
                   rdfReader: RDFReader,
                   inferOptions: InferOptions
                  ): Either[String, ShExSchema]  = for {
    es <- m.map {
      case (iri, is) => mkShExShape(is,iri,rdfReader,inferOptions)
    }.toList.sequence[ES,ShapeExpr]
  } yield {
    val pm =
      addPrefix("schema",schemaIri,inferOptions, _.addSchemaAlias,m,hasSchema)(
      addPrefix("wdp",propIri,inferOptions, _.addWDPropAlias,m,hasWDProp)(
      addPrefix("xsd",xsd,inferOptions, _.addXSDAlias,m,hasXSD)(
      addPrefix("rdf",rdf,inferOptions, _.addRDFAlias,m,hasRDF)(
      addPrefix("sx",sx,inferOptions, _.addSxAlias,m,hasStar)(
      addPrefix("rdfs",rdfs,inferOptions,_.addRDFSAlias,m,_ => true)(
        rdfReader.getPrefixMap
      ))))))

    ShExSchema(Schema(IRI(""),Some(pm), None, None, None, Some(es), None, List()))
  }

  private case class InferredValue(constraint: InferredNodeConstraint,
                                   number: Int)

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