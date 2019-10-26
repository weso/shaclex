package es.weso.schemaInfer
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.schema._
import es.weso.schema.Schemas._
import es.weso.shapeMaps._

object SchemaInfer {

  private type NeighMap = Map[IRI, Set[RDFNode]]

  private type Err = String
  private type R[A] = ReaderT[Id,Config,A]
  private type S[A] = StateT[R, InferState,A]
  private type Comp[A] = EitherT[S,Err,A]

  private def ok[A](x:A): Comp[A] = EitherT.pure(x)

  private def getState: Comp[InferState] = EitherT.liftF(StateT.get)

  private def getSchema: Comp[InferredSchema] = getState.map(_.schema)

  private def getPrefixMap: Comp[PrefixMap] = getState.map(_.prefixMap)

  private def getShapeMap: Comp[ResultShapeMap] = getState.map(_.inferredShapeMap)

  private def updateState(fn: InferState => InferState): Comp[Unit] =
    EitherT.liftF(StateT.modify(fn))

//  private def setState(s: InferState): Comp[Unit] = EitherT.liftF(StateT.set(s))

  private def updateSchema(fn: InferredSchema => InferredSchema): Comp[Unit] =
    updateState(_.updateSchema(fn))

  private def updateShapeMap(fn: ResultShapeMap => ResultShapeMap): Comp[Unit] =
    updateState(_.updateShapeMap(fn))

  private def addPrefixDecl(prefix: Prefix, iri: IRI): Comp[Unit] =
    updateState(_.addPrefixDecl(prefix,iri))

  private def err[A](msg:String): Comp[A] = EitherT.leftT[S,A](msg)

  private def sequence[A](ls: List[Comp[A]]): Comp[List[A]] = ls.sequence[Comp,A]

  private def runWithState[A](c: Comp[A], initial: InferState, config: Config): Either[String,(A,InferState)] = {
    val (finalState, x) = c.value.run(initial).run(config)
    x.map(v => (v,finalState))
  }

  private def getConfig: Comp[Config] = EitherT.liftF(StateT.liftF(ReaderT.ask))
  private def getRDF:Comp[RDFReader] = getConfig.map(_.rdf)
  private def getOptions:Comp[InferOptions] = getConfig.map(_.options)
  private def fromES[A](e: Either[String,A]): Comp[A] = EitherT.fromEither(e)

  private def addShape(lbl: IRI, shape: InferredShape): Comp[Unit] = for {
    _ <- updateSchema(schema => schema.get(lbl) match {
      case None => schema.updated(lbl,shape)
      case Some(previousShape) => schema.updated(lbl, previousShape.collapse(shape))
    })
  } yield ()

  type InferredRow = (IRI,InferredNodesValue)
  type SortFunction = PrefixMap => (InferredRow,InferredRow) => Boolean

  def runInferSchema(rdfReader:RDFReader,
                     selector: NodeSelector,
                     engine: String,
                     shapeLabel: IRI,
                     opts: InferOptions = InferOptions.defaultOptions
                    ): Either[String, (Schema, ResultShapeMap)] = {
    runWithState(inferSchema(selector, engine, shapeLabel),
                 InferState.initial.addPrefixMap(rdfReader.getPrefixMap),
                 Config(opts, rdfReader)).map {
      case (schema, state) => (schema, state.inferredShapeMap)
    }
  }

  private def inferSchema(selector: NodeSelector,
                  engine: String,
                  shapeLabel: IRI,
                 ): Comp[Schema] = for {
    rdfReader <- getRDF
    nodes <- fromES(selector.select(rdfReader))
//    neighMaps <- sequence(nodes.toList.map(getNeighbourhood(_)))
//    _ <- sequence(neighMaps.map(n => inferShape(shapeLabel,n)))
    _ <- associateNodesLabel(nodes,shapeLabel)
    _ <- inferShapeFromNodes(nodes, shapeLabel, 0)
    schema <- mkSchema(engine, shapeLabel)
    _ <- updateShapeMap(_.addNodesPrefixMap(rdfReader.getPrefixMap()).addShapesPrefixMap(schema.pm))
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

  private def inferShapeFromNeighMap(shapeLabel: IRI,
                                     neighMap: NeighMap,
                                     inferredValues: InferredNodeValue,
                                     numFollowOns: Int
                                    ): Comp[Option[InferredShape]] =
  if (neighMap.isEmpty) {
    // println(s"inferShapeFromNeighMap(shapeLabel=$shapeLabel, inferredValues: $inferredValues): EmptyNeighMap")
    ok(None)
  }
  else {
    // println(s"inferShapeFromNeighMap(shapeLabel=$shapeLabel, neighMap=$neighMap, inferredValues: $inferredValues)")
//    println(s"Inferring shape for $shapeLabel with neighMap: $neighMap")
    for {
      rows <- sequence(
        neighMap
          .map {
            case (iri, nodes) => (iri, inferValue(shapeLabel, iri, nodes, numFollowOns))
          }
          .toList
          .map(mkRow))
      shape = rows.toMap
 //     _ <- { println(s"inferShape($shapeLabel)=$shape"); ok(()) }
      is = InferredShape(SingleNodesValue(inferredValues), shape)
      _ <- addShape(shapeLabel, is)
    } yield Some(is)
  }

  private def inferValue(shapeLabel: IRI,
                         iri: IRI,
                         nodes: Set[RDFNode],
                         numFollowOns: Int
                        ): Comp[InferredNodesValue] = {
 //   println(s"inferValue(shapeLabel=$shapeLabel, iri=$iri, nodes=$nodes")
    for {
      opts <- getOptions
      maybeLs <- sequence(opts.followOnLs.map(fo => followOn(fo, shapeLabel, iri, nodes, numFollowOns)))
//      _ <- { println(s"Result of followOn: $maybeLs"); ok(())}
      srefs <- collect(maybeLs)
      iv <- srefs.length match {
        case 0 => collapse(nodes)
        case 1 => ok(InferredNodeValue(Ref(srefs.head),nodes.size))
        case _ => err(s"More than one followOn: $srefs")
      }
      _ <- checkPrefix(iri, iv)
    } yield SingleNodesValue(iv)
  }

  private def collect[A](ls: List[Option[A]]): Comp[List[A]] = {
    val es = ls.collect { case Some(x) => x }
    ok(es)
/*    es.length match {
      case 1 => ok(Some(es.head))
      case 0 => ok(None)
      case _ => {
//        println(s"More than one followOn predicate: $es ")
        ok(Some(es.head))
      }
    } */
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

  private def associateNodeLabel(node: RDFNode, label: IRI): Comp[Unit] = for {
    state <- getState
    newState  <- fromES(state.addNodeLabel(node,label))
    _ <- updateState(_ => newState)
  } yield (())

  private def associateNodesLabel(nodes: Set[RDFNode], label: IRI): Comp[Unit] = {
    // println(s"AssociateNodesLabel: $nodes -> $label")
    sequence(nodes.toList.map(node => associateNodeLabel(node, label))).map(_ => ())
  }

  private def inferShapeFromNodes(nodes: Set[RDFNode],
                                  shapeLabel: IRI,
                                  numFollowOns: Int
                                 ): Comp[Option[IRI]] = for {
    _ <- { println(s"inferShapeFromNodes(nodes=$nodes, label=$shapeLabel, numFollowOns=$numFollowOns"); ok(()) }
    neighMaps <- sequence(nodes.toList.map(getNeighbourhood(_, numFollowOns)))
    ivalues <- collapse(nodes)
    lsShapes <- sequence(neighMaps.map(n => inferShapeFromNeighMap(shapeLabel, n, ivalues, numFollowOns + 1)))
    v <- if (lsShapes.exists(_.isDefined))
      for {
        _ <- associateNodesLabel(nodes,shapeLabel)
      } yield Some(shapeLabel)
      else /* for {
      // println(s"inferShapeFromNodes($shapeLabel)...none")
      _ <- associateNodesLabel(nodes,shapeLabel)
      } yield Some(shapeLabel) */
       ok(None)
  } yield v

  private def followOn(fo: FollowOn,
                       shapeLabel: IRI,
                       iri: IRI,
                       nodes: Set[RDFNode],
                       numFollowOn: Int
                      ): Comp[Option[IRI]] = {
    fo.check(shapeLabel,iri, numFollowOn).fold(e => {
      // println(s"$e");
      ok(None)
    }, newLabel => {
      // println(s"Matches $iri with $prop")
      for {
       rdf <- getRDF
       maybeShape <- inferShapeFromNodes(nodes, newLabel, numFollowOn)
      } yield maybeShape
    })
  }

/*  private def getNeighbourhoods(nodes: Set[RDFNode],
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
  } */

  private def isVisited(node: RDFNode): Comp[Boolean] = for {
   s <- getState
  } yield s.isVisited(node)

  private def addVisited(node: RDFNode): Comp[Unit] = updateState(_.addVisited(node))

  private def getMaxFollowOns: Comp[Int] = getOptions.map(_.maxFollowOn)

  private def getNeighbourhood(node: RDFNode,
                               numberFollowOns: Int
                              ): Comp[NeighMap] = for {
      b <- isVisited(node)
      maxFollowOns <- getMaxFollowOns
//      s <- getState
//      _ <- { println(s"getNeighbourhood($node). visited?: $b, visited: $s"); ok(())}
      nm <- if (b || numberFollowOns >= maxFollowOns) ok(Map[IRI,Set[RDFNode]]())
      else for {
      rdf <- getRDF
      ts <- fromES(rdf.triplesWithSubject(node))
      _ <- addVisited(node)
    } yield ts.map(
      triple => (triple.pred, triple.obj)
    ).groupBy(_._1).map { case (k,v) => (k, v.map(_._2))}
  } yield nm

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

/*  private def hasStar(c: InferredNodesValue): Boolean =
    c.number > 1 */

/*  private def containsDeclSchema(m: InferredSchema,
                           check: InferredNodesValue => Boolean,
                           name: String): Boolean = {
    m.values.exists(containsDeclShape(check,name))
  }

  private def containsDeclShape(check: InferredNodesValue => Boolean,
                                  name: String)(m: InferredShape): Boolean = {
    m.values.exists(check)
  }
*/
/*  private def addPrefix(alias: String,
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
  } */

  private def mkSchema(engine: String, label: IRI): Comp[Schema] =
   lookupSchema(engine) match {
    case Right(_: ShExSchema) => mkShExSchema(label)
    case Right(_: ShaclexSchema) => err(s"Not implemented yet")
    case Left(e) => err(s"Not found engine $engine, Error: $e")
    case l => err(s"Not found schema $engine. Found: $l")
  }

  private def mkShExSchema(label: IRI): Comp[Schema]  = for {
    rdfReader <- getRDF
    opts <- getOptions
    schema <- getSchema
    pm <- getPrefixMap
    shexSchema <- fromES(schema.toShExSchema(rdfReader,opts,pm))
  } yield {
    ShExSchema(shexSchema)
  }



}