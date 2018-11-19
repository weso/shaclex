package es.weso.schemaInfer
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.{Prefix, PrefixMap, RDFReader}
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.schema.{Schema, ShExSchema, ShaclexSchema}
import es.weso.schema.Schemas._
import es.weso.shapeMaps.NodeSelector
import es.weso.shex._
import es.weso.rdf.triples.RDFTriple
import FollowOn._

object SchemaInfer {

  // Namespace for annotations
  val sx = IRI("http://weso.es/ns/shex/")
  val `sx:maxNumber` = sx + "maxNumber"
  val wikidataId = "Endpoint(https://query.wikidata.org/sparql)"
  val wikibase = IRI("http://wikiba.se/ontology#")
  val schemaIri = IRI("http://schema.org/")
  val wikidata = IRI("http://www.wikidata.org/")
  val propIri   = wikidata + "prop/"
  val wdt = wikidata + "prop/direct/"
  val propRef = wikidata + "prop/reference/"
  val propRefValue = wikidata + "prop/reference/value/"
  val skos = IRI("http://www.w3.org/2004/02/skos/core#")
  val wdRef = IRI("http://www.wikidata.org/reference/")
  val `wikibase:directClaim` = wikibase + "directClaim"

  case class PrefixDecl(alias: String, IRI: IRI)
  val possiblePrefixes = PrefixMap(Map(
    Prefix("cc") -> IRI("http://creativecommons.org/ns#"),
    Prefix("dct") -> IRI("http://purl.org/dc/terms/"),
    Prefix("owl") -> IRI("http://www.w3.org/2002/07/owl#"),
    Prefix("pq") -> IRI("http://www.wikidata.org/prop/qualifier/"),
    Prefix("pqv") -> IRI("http://www.wikidata.org/prop/qualifier/value/"),
    Prefix("pqvn") -> IRI("http://www.wikidata.org/prop/qualifier/value-normalized/"),
    Prefix("pr") -> propRef,
    Prefix("prn") -> IRI("http://www.wikidata.org/prop/reference/value-normalized/"),
    Prefix("prv") -> propRefValue,
    Prefix("prov") -> prov,
    Prefix("ps") -> IRI("http://www.wikidata.org/prop/statement/"),
    Prefix("psv") -> IRI("http://www.wikidata.org/prop/statement/value/"),
    Prefix("psvn") -> IRI("http://www.wikidata.org/prop/statement/value-normalized/"),
    Prefix("rdf") -> rdf,
    Prefix("rdfs") -> rdfs,
    Prefix("schema") -> schemaIri,
    Prefix("skos") -> skos,
    Prefix("sx") -> sx,
    Prefix("wdno") -> IRI("http://www.wikidata.org/prop/novalue/"),
    Prefix("wdt") -> wdt,
    Prefix("wdtn") -> IRI("http://www.wikidata.org/prop/direct-normalized/"),
    Prefix("wdp") -> propIri,
    Prefix("wdref") -> wdRef,
    Prefix("wikibase") -> wikibase,
    Prefix("wdata") -> IRI("http://www.wikidata.org/wiki/Special:EntityData/"),
    Prefix("xsd") -> xsd
  ))

  private type NeighMap = Map[IRI, Set[RDFNode]]
  private type EitherNeighMap = Either[String,NeighMap]

  private type InferredShape = Map[IRI, InferredNodesValue]
  private type InferredSchema = Map[IRI, InferredShape]

  private case class State(schema: InferredSchema, prefixMap: PrefixMap) {
    def updateSchema(fn: InferredSchema => InferredSchema): State =
      this.copy(schema = fn(schema))

    def addPrefixMap(pm: PrefixMap): State = this.copy(prefixMap = this.prefixMap.merge(pm))

    def addPrefixDecl(p: Prefix, iri: IRI): State = this.copy(prefixMap = prefixMap.addPrefix(p,iri))
  }

  private object State {
    def initial: State = State(Map(),PrefixMap.empty.addPrefix(Prefix("sx"), sx))
  }

  private case class Config(options: InferOptions, rdf: RDFReader)

  private type Err = String
  private type R[A] = ReaderT[Id,Config,A]
  private type S[A] = StateT[R,State,A]
  private type Comp[A] = EitherT[S,Err,A]
  private type ES[A] = Either[String,A]

  private def ok[A](x:A): Comp[A] = EitherT.pure(x)

  private def getState: Comp[State] = EitherT.liftF(StateT.get)

  private def getSchema: Comp[InferredSchema] = getState.map(_.schema)

  private def getPrefixMap: Comp[PrefixMap] = getState.map(_.prefixMap)

  private def updateState(fn: State => State): Comp[Unit] =
    EitherT.liftF(StateT.modify(fn))

  private def setState(s: State): Comp[Unit] = EitherT.liftF(StateT.set(s))

  private def updateSchema(fn: InferredSchema => InferredSchema): Comp[Unit] =
    updateState(_.updateSchema(fn))

  private def addPrefixDecl(prefix: Prefix, iri: IRI): Comp[Unit] =
    updateState(_.addPrefixDecl(prefix,iri))

  private def err[A](msg:String): Comp[A] = EitherT.leftT[S,A](msg)

  private def sequence[A](ls: List[Comp[A]]): Comp[List[A]] = ls.sequence[Comp,A]

  private def runWithState[A](c: Comp[A], initial: State, config: Config): Either[String,A] = {
    val (_,x) = c.value.run(initial).run(config)
    x
  }

  private def getConfig: Comp[Config] = EitherT.liftF(StateT.liftF(ReaderT.ask))
  private def getRDF:Comp[RDFReader] = getConfig.map(_.rdf)
  private def getOptions:Comp[InferOptions] = getConfig.map(_.options)
  private def fromES[A](e: Either[String,A]): Comp[A] = EitherT.fromEither(e)

  private def addShape(lbl: IRI, shape: InferredShape): Comp[Unit] = for {
    _ <- updateSchema(schema => schema.get(lbl) match {
      case None => schema.updated(lbl,shape)
      case Some(previousShape) => schema.updated(lbl, collapseShapes(previousShape,shape))
    })
  } yield ()

  private def collapseShapes(shape1: InferredShape, shape2: InferredShape): InferredShape = {
    val zero = shape1
    def cmb(s: InferredShape, current: (IRI, InferredNodesValue)): InferredShape = {
       val (iri,iv) = current
       s.get(iri) match {
         case None => s.updated(iri,iv)
         case Some(other) => s.updated(iri, iv.collapse(other))
       }
    }
    shape2.foldLeft(zero)(cmb)
  }

  type InferredRow = (IRI,InferredNodesValue)
  type SortFunction = PrefixMap => (InferredRow,InferredRow) => Boolean

  case class InferOptions(
   inferTypePlainNode: Boolean,
   addLabelLang: Option[Lang],
   possiblePrefixMap: PrefixMap,
   followOnLs: List[FollowOn],
   sortFunction: SortFunction
  )
  object InferOptions {

    def orderByIRI: SortFunction = pm => (pair1,pair2) => {
      val (iri1,_) = pair1
      val (iri2,_) = pair2
      pm.qualify(iri1) < pm.qualify(iri2)
    }

    val defaultOptions =
      InferOptions(
        inferTypePlainNode = true,
        addLabelLang = Some(Lang("en")),
        possiblePrefixMap = possiblePrefixes,
        followOnLs = List(followOnReference, followOnWasDerivedFrom),
        sortFunction = orderByIRI
      )
  }

  def runInferSchema(rdfReader:RDFReader,
                     selector: NodeSelector,
                     engine: String,
                     shapeLabel: IRI,
                     opts: InferOptions = InferOptions.defaultOptions
                    ): Either[String, Schema] = {
    runWithState(inferSchema(selector, engine, shapeLabel),
                 State.initial.addPrefixMap(rdfReader.getPrefixMap),
                 Config(opts, rdfReader)
    )
  }

  private def inferSchema(selector: NodeSelector,
                  engine: String,
                  shapeLabel: IRI,
                 ): Comp[Schema] = for {
    rdfReader <- getRDF
    nodes <- fromES(selector.select(rdfReader))
//    neighMaps <- sequence(nodes.toList.map(getNeighbourhood(_)))
//    _ <- sequence(neighMaps.map(n => inferShape(shapeLabel,n)))
    _ <- inferShapeFromNodes(nodes, shapeLabel)
    schema <- mkSchema(engine, shapeLabel)
  } yield schema

/*  private def inferShapes(neighMap: NeighMap,
                          engine: String,
                          shapeLabel: IRI
                         ): Comp[Unit] = {
    val zero: Comp[Unit] = ok(())
    def cmb(schema: Comp[Unit], current: (IRI,Set[RDFNode])): Comp[Unit] = for {
      shape <- inferShape(shapeLabel, current)
      _ <- updateSchema(_.updated(shapeLabel,shape))
    } yield Map(shapeLabel -> shape)

    neighMap.foldLeft(zero)(cmb)
  } */

  private def mkRow(pair: (IRI, Comp[InferredNodesValue])): Comp[(IRI, InferredNodesValue)] = {
    val (iri,comp) = pair
    for {
      iv <- comp
    } yield (iri,iv)
  }

  private def inferShape(shapeLabel: IRI, neighMap: NeighMap): Comp[InferredShape] = for {
   rows <- sequence(neighMap.map{
     case (iri, nodes) => (iri, inferValue(shapeLabel, iri, nodes))
   }.toList.map(mkRow))
   shape = rows.toMap
   _ <- addShape(shapeLabel, shape)
  } yield shape

  private def inferValue(shapeLabel: IRI,
                         iri: IRI,
                         nodes: Set[RDFNode]
                        ): Comp[InferredNodesValue] = {
    for {
      opts <- getOptions
      maybeLs <- sequence(opts.followOnLs.map(fo => followOn(fo,shapeLabel,iri,nodes)))
//      _ <- { println(s"Result of followOn: $maybeLs"); ok(())}
      maybeFollowOn <- existsSome(maybeLs)
      iv <- maybeFollowOn match {
        case None      => collapse(nodes)
        case Some(lbl) => ok(InferredNodeValue(Ref(lbl),1))
      }
      _ <- checkPrefix(iri, iv)
    } yield SingleNodesValue(iv)
  }

  private def existsSome[A](ls: List[Option[A]]): Comp[Option[A]] = {
    val es = ls.collect { case Some(x) => x }
    es.length match {
      case 1 => ok(Some(es.head))
      case 0 => ok(None)
      case _ => {
//        println(s"More than one followOn predicate: $es ")
        ok(Some(es.head))
      }
    }
  }

  private def checkPrefix(iri: IRI,iv: InferredNodeValue): Comp[Unit] = for {
    _ <- checkPrefixIRI(iri)
    _ <- checkPrefixIv(iv)
  } yield (())

  private def checkPrefixIRI(iri: IRI): Comp[Unit] = for {
    options <- getOptions
    _ <- options.possiblePrefixMap.getPrefixLocalName(iri).fold(
      _ => ok(()),
      { case (prefix, iri, localName) => addPrefixDecl(prefix,iri) }
    )
  } yield (())

  private def checkPrefixIv(iv: InferredNodeValue): Comp[Unit] =
    iv.getIRI match {
      case None => ok(())
      case Some(iri) => checkPrefixIRI(iri)
  }

  private def inferShapeFromNodes(nodes: Set[RDFNode], shapeLabel: IRI): Comp[Unit] = for {
    neighMaps <- sequence(nodes.toList.map(getNeighbourhood(_)))
    _ <- sequence(neighMaps.map(n => inferShape(shapeLabel,n)))
  } yield (())

  private def followOn(fo: FollowOn, shapeLabel: IRI, iri: IRI, nodes: Set[RDFNode]): Comp[Option[IRI]] = {
    fo.check(shapeLabel,iri).fold(e => {
      // println(s"$e");
      ok(None)
    }, newLabel => {
      // println(s"Matches $iri with $prop")
      for {
       rdf <- getRDF
       _ <- inferShapeFromNodes(nodes, newLabel)
      } yield (Some(newLabel))
    })
  }

  private def getNeighbourhoods(nodes: Set[RDFNode],
                        rdf: RDFReader
                       ): Comp[NeighMap] = {
    val zero: EitherNeighMap = Right(Map())
    def combine(rest: EitherNeighMap,
                node: RDFNode
               ): EitherNeighMap = for {
     ts <- rdf.triplesWithSubject(node)
    } yield ts.map(
       triple => (triple.pred, triple.obj)
     ).groupBy(_._1).map { case (k,v) => (k, v.map(_._2))}
    val e = nodes.foldLeft(zero)(combine)
    fromES(e)
  }

  private def getNeighbourhood(node: RDFNode
                              ): Comp[NeighMap] = for {
      rdf <- getRDF
      ts <- fromES(rdf.triplesWithSubject(node))
    } yield ts.map(
      triple => (triple.pred, triple.obj)
    ).groupBy(_._1).map { case (k,v) => (k, v.map(_._2))}


  private def collapse(nodes: Set[RDFNode]): Comp[InferredNodeValue] = for {
   opts <- getOptions
  } yield {
   if (nodes.isEmpty) InferredNodeValue(InferredNone,0)
   else {
     def zero = inferNode(nodes.head, opts)
     def combine(rest: InferredNodeConstraint, node: RDFNode): InferredNodeConstraint = {
       collapsePair(rest,node)
     }
     val c = nodes.tail.foldLeft(zero)(combine)
     InferredNodeValue(c,nodes.size)
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
                  ): InferredNodeConstraint = nk.collapseNode(node)

  private def hasStar(c: InferredNodesValue): Boolean =
    c.number > 1

  private def containsDeclSchema(m: InferredSchema,
                           check: InferredNodesValue => Boolean,
                           name: String): Boolean = {
    !m.values.filter(containsDeclShape(check,name)).isEmpty
  }

  private def containsDeclShape(check: InferredNodesValue => Boolean,
                                  name: String)(m: InferredShape): Boolean = {
    !m.values.filter(check).isEmpty
  }

  private def addPrefix(alias: String,
                        iri: IRI,
                        options: InferOptions,
                        checkOptions: InferOptions => Boolean,
                        m: InferredSchema,
                        check: InferredNodesValue => Boolean)(pm: PrefixMap): PrefixMap = {
    if (containsDeclSchema(m, check, alias)) {
      if (checkOptions(options) && !pm.contains(alias)) pm.addPrefix(alias,iri)
      else pm
    }
    else pm
  }

  private def mkSchema(engine: String, label: IRI): Comp[Schema] =
   lookupSchema(engine) match {
    case Right(_: ShExSchema) => mkShExSchema(label)
    case Right(_: ShaclexSchema) => err(s"Not implemented yet")
    case Left(e) => err(s"Not found engine $engine, Error: $e")
    case l => err(s"Not found schema $engine. Found: $l")
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
       labels <- sourceIRIs.map(rdf.triplesWithSubjectPredicate(_,`rdfs:label`)).toList.sequence[ES,Set[RDFTriple]]
     } yield labels.flatten.map(_.obj)
     else {
      rdf.triplesWithSubjectPredicate(iri, `rdfs:label`).map(_.obj)
     }
  } yield {
    val okLabels = labels.collect {
      case l: Literal if hasLang(l,maybeLang) => l
    }
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
                           c: InferredNodesValue,
                           rdf: RDFReader,
                           inferOptions: InferOptions
                          ): Either[String, TripleExpr] = for {
   label <- getLabel(iri, rdf, inferOptions.addLabelLang)
    // _ <- { println(s"Selected label for $iri: $label") ; Right(()) }
  } yield {
    val labelAnnotation: Option[Annotation] =
      label.map(lbl => Annotation(`rdfs:label`, ObjectValue.literalValue(lbl)))
    val totalNumberAnnotation: Option[Annotation] =
      if (c.number > 1) Some(Annotation(`sx:maxNumber`, ObjectValue.intValue(c.number))) else None
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
    es <- m.toList.sortWith(inferOptions.sortFunction(inferOptions.possiblePrefixMap)).map {
      case (iri, c) => mkTripleExpr(iri,c,rdfReader,inferOptions)
    }.sequence[ES,TripleExpr]
  } yield {
    val shapeLabel = IRILabel(label)
    val expr: Option[TripleExpr] = es.length match {
      case 0 => None
      case 1 => Some(es.head)
      case _ => Some(EachOf(None, es, None, None, None, None))
    }
    Shape(Some(shapeLabel),None,None,None,expr,None,None,None)
  }

  private def mkShExSchema(label: IRI): Comp[Schema]  = for {
    rdfReader <- getRDF
    opts <- getOptions
    schema <- getSchema
    es <- fromES(schema.map {
      case (iri, is) => mkShExShape(is,iri,rdfReader,opts)
    }.toList.sequence[ES,ShapeExpr])
    pm <- getPrefixMap
  } yield {
/*    val pm =
      addPrefix("schema",schemaIri,opts, _.addSchemaAlias,schema,hasSchema)(
      addPrefix("skos",skos,opts, _.addSkosAlias,schema,hasSkos)(
      addPrefix("wikibase",wikibase,opts, _.addWikibaseAlias,schema,hasWikibase)(
      addPrefix("wdt",wdt,opts, _.addWDDirect,schema,hasWDDirect)(
      addPrefix("wdp",propIri,opts, _.addWDPropAlias,schema,hasWDProp)(
      addPrefix("pr",propRef,opts, _.addPropRef,schema,hasPropRef)(
      addPrefix("prv",propRefValue,opts, _.addPropRefValue,schema,hasPropRefValue)(
      addPrefix("xsd",xsd,opts, _.addXSDAlias,schema,hasXSD)(
      addPrefix("rdf",rdf,opts, _.addRDFAlias,schema,hasRDF)(
      addPrefix("sx",sx,opts, _.addSxAlias,schema,hasStar)(
      addPrefix("rdfs",rdfs,opts,_.addRDFSAlias,schema,_ => true)(
        rdfReader.getPrefixMap
      ))))))))))) */

    ShExSchema(Schema(IRI(""),Some(pm), None, None, None, Some(es), None, List()))
  }




  private def mkShExConstraint(c: InferredNodeConstraint, options: InferOptions): ShapeExpr = c match {
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
    case InferredString => NodeConstraint.datatype(`xsd:string`, List())
    case InferredLang(lang) => NodeConstraint.valueSet(List(LanguageStem(lang)), List())
    case InferredLangString => NodeConstraint.datatype(`rdf:langString`, List())
    case InferredDatatype(dt) => NodeConstraint.datatype(dt, List())
    case InferredNone => NodeConstraint.empty
    case Ref(lbl) => {
      // println(s"Ref($lbl")
      ShapeRef(IRILabel(lbl),None,None)
    }
  }

  private def mkMinCardinality(n: Integer): Integer =
    if (n == 1) 1
    else 0

  private def mkMaxCardinality(n: Integer): Max =
    if (n == 1) IntMax(1)
    else Star

}