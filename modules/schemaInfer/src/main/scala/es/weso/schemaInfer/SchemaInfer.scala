package es.weso.schemaInfer
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.{PrefixMap, RDFReader}
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
  val propIri   = IRI("http://www.wikidata.org/prop/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val skos = IRI("http://www.w3.org/2004/02/skos/core#")
  val `wikibase:directClaim` = wikibase + "directClaim"

  private type NeighMap = Map[IRI, Set[RDFNode]]
  private type EitherNeighMap = Either[String,NeighMap]

  private type InferredShape = Map[IRI, InferredNodesValue]
  private type InferredSchema = Map[IRI, InferredShape]

  private case class State(schema: InferredSchema) {
    def updateSchema(fn: InferredSchema => InferredSchema): State =
      this.copy(schema = fn(schema))
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
  private def updateSchema(fn: InferredSchema => InferredSchema): Comp[Unit] =
    EitherT.liftF(StateT.modify(_.updateSchema(fn)))
  private def setState(s: State): Comp[Unit] = EitherT.liftF(StateT.set(s))
  private def err[A](msg:String): Comp[A] = EitherT.leftT[S,A](msg)
  private def sequence[A](ls: List[Comp[A]]): Comp[List[A]] = ls.sequence[Comp,A]
  private def runWithState[A](c: Comp[A], initial: State, config: Config): Either[String,A] =
    c.value.run(initial).run(config)._2

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

  private def collapseValues(iv1: InferredNodeValue, iv2: InferredNodeValue): InferredNodeValue = {
   ???
  }



  case class InferOptions(
   inferTypePlainNode: Boolean,
   addXSDAlias: Boolean,
   addRDFAlias: Boolean,
   addSxAlias: Boolean,
   addRDFSAlias: Boolean,
   addWDPropAlias: Boolean,
   addSchemaAlias: Boolean,
   addSkosAlias: Boolean,
   addWikibaseAlias: Boolean,
   addWDDirect: Boolean,
   addLabelLang: Option[Lang],
   followOnLs: List[FollowOn]
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
        addWDDirect = true,
        addSkosAlias = true,
        addWikibaseAlias = true,
        addLabelLang = Some(Lang("en")),
        followOnLs = List(followOnReference)
      )
  }

  def runInferSchema(rdfReader:RDFReader,
                     selector: NodeSelector,
                     engine: String,
                     shapeLabel: IRI,
                     opts: InferOptions = InferOptions.defaultOptions): Either[String, Schema] =
    runWithState(inferSchema(selector, engine, shapeLabel),
      State(Map()),
      Config(opts,rdfReader)
    )


  private def inferSchema(selector: NodeSelector,
                  engine: String,
                  shapeLabel: IRI,
                 ): Comp[Schema] = for {
    rdfReader <- getRDF
    nodes <- fromES(selector.select(rdfReader))
    neighMaps <- sequence(nodes.toList.map(getNeighbourhood(_)))
    _ <- sequence(neighMaps.map(n => inferShape(shapeLabel,n)))
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
     case (iri, nodes) => (iri, inferValue(iri, nodes))
   }.toList.map(mkRow))
   shape = rows.toMap
   _ <- addShape(shapeLabel, shape)
  } yield shape

  private def inferValue(iri: IRI, nodes: Set[RDFNode]): Comp[InferredNodesValue] = {
    for {
      opts <- getOptions
      _ <- sequence(opts.followOnLs.map(fo => followOn(fo,iri,nodes)))
      iv <- collapse(nodes)
    } yield SingleNodesValue(iv)
  }

  private def followOn(fo: FollowOn, iri: IRI, nodes: Set[RDFNode]): Comp[Unit] = {
    if (fo.check(iri)) {
      println(s"Checking followOn $iri")
      for {
       rdf <- getRDF
       ts <- fromES(nodes.toList.map(node =>
         rdf.triplesWithSubjectPredicate(node,fo.pred).
           map(_.toList)
       ).sequence[ES,List[RDFTriple]].map(_.flatten))
       _ <- { println(s"Triples from followOn: $ts"); ok(()) }
      } yield ()
    } else ok(())
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

  private def hasXSD(c: InferredNodesValue): Boolean = c.constraint match {
    case InferredString => true
    case InferredDatatype(dt) =>
      if (dt.getLexicalForm.startsWith(xsd.str)) true
      else false
    case _ => false
  }

  private def hasWDProp(c: InferredNodesValue) =  true
  private def hasSchema(c: InferredNodesValue) =  true
  private def hasWikibase(c: InferredNodesValue) =  true
  private def hasWDDirect(c: InferredNodesValue) = true
  private def hasSkos(c: InferredNodesValue) = true


  private def hasRDF(c: InferredNodesValue): Boolean = c.constraint match {
    case InferredLangString => true
    case _ => false
  }
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
//       _ <- { println(s"sourceIRIs for $iri: $sourceIRIs"); Right(()) }
       labels <- sourceIRIs.map(rdf.triplesWithSubjectPredicate(_,`rdfs:label`)).toList.sequence[ES,Set[RDFTriple]]
//       _ <- { println(s"All labels for $iri: $labels"); Right(()) }
     } yield labels.flatten.map(_.obj)
     else {
      rdf.triplesWithSubjectPredicate(iri, `rdfs:label`).map(_.obj)
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

  private def mkShExSchema(label: IRI): Comp[Schema]  = for {
    rdfReader <- getRDF
    opts <- getOptions
    schema <- getSchema
    es <- fromES(schema.map {
      case (iri, is) => mkShExShape(is,iri,rdfReader,opts)
    }.toList.sequence[ES,ShapeExpr])
  } yield {
    val pm =
      addPrefix("schema",schemaIri,opts, _.addSchemaAlias,schema,hasSchema)(
      addPrefix("skos",skos,opts, _.addSkosAlias,schema,hasSkos)(
      addPrefix("wikibase",wikibase,opts, _.addWikibaseAlias,schema,hasWikibase)(
      addPrefix("wdt",wdt,opts, _.addWDDirect,schema,hasWDDirect)(
      addPrefix("wdp",propIri,opts, _.addWDPropAlias,schema,hasWDProp)(
      addPrefix("xsd",xsd,opts, _.addXSDAlias,schema,hasXSD)(
      addPrefix("rdf",rdf,opts, _.addRDFAlias,schema,hasRDF)(
      addPrefix("sx",sx,opts, _.addSxAlias,schema,hasStar)(
      addPrefix("rdfs",rdfs,opts,_.addRDFSAlias,schema,_ => true)(
        rdfReader.getPrefixMap
      )))))))))

    ShExSchema(Schema(IRI(""),Some(pm), None, None, None, Some(es), None, List()))
  }




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
    case InferredString => NodeConstraint.datatype(`xsd:string`, List())
    case InferredLang(lang) => NodeConstraint.valueSet(List(LanguageStem(lang)), List())
    case InferredLangString => NodeConstraint.datatype(`rdf:langString`, List())
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