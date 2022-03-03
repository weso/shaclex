package es.weso.shex.converter

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import es.weso.shacl._
//import es.weso.shex.{Shape => ShExShape, _}
import es.weso.shapemaps.QueryShapeMap
import es.weso.shex.{Annotation, Direct, Inverse, NodeConstraint, Path, ValueSetValue}

object ShEx2Shacl extends LazyLogging {

  def shex2Shacl(schema: shex.Schema, shapeMap: Option[QueryShapeMap]): Either[List[String], shacl.Schema] = {
    runWithState(getSchema(schema), initialState, schema)
  }

  private type Err            = String
  private type ShapesMap      = Map[shacl.RefNode, shacl.Shape]
  private type ShapeExprsMap  = Map[shex.ShapeExpr, shacl.RefNode]
  private type TripleExprsMap = Map[shex.TripleExpr, shacl.RefNode]

  private case class State(
      shapesMap: ShapesMap,
      shapeExprsMap: ShapeExprsMap,
      tripleExprsMap: TripleExprsMap,
      counter: Int
  ) {

    def addShapeRefShape(sref: RefNode, s: shacl.Shape): State =
      this.copy(shapesMap = shapesMap.updated(sref, s))

    def newShapeExprRef(se: shex.ShapeExpr): (State, shacl.RefNode) = {
      val id = RefNode(BNode("B" + counter))
      (
        this.copy(
          shapeExprsMap = shapeExprsMap.updated(se, id),
          counter = counter + 1
        ),
        id
      )
    }

    def newTripleExprRef(te: shex.TripleExpr): (State, shacl.RefNode) = {
      val id = RefNode(BNode("B" + counter))
      (
        this.copy(
          tripleExprsMap = tripleExprsMap.updated(te, id),
          counter = counter + 1
        ),
        id
      )
    }

    def getShapeRef(sref: RefNode): Option[shacl.Shape] =
      this.shapesMap.get(sref)

  }

  private type S[A]      = StateT[Id, State, A]
  private type RS[A]     = ReaderT[S, shex.Schema, A]
  private type Result[A] = EitherT[RS, List[Err], A]

  private def ok[A](x: A): Result[A] = {
    EitherT.pure(x)
  }

  private def err[A](msg: String): Result[A] = {
    EitherT.fromEither(List(msg).asLeft[A])
  }

  private def info(msg: String): Result[Unit] = {
    logger.info(s"$msg")
    ok(())
  }

//  private def str2err(str: String): List[Err] = List(str)

  /*  private def fromEitherString[A](e: Either[String,A]): Result[A] =
    EitherT.fromEither[S](e).leftMap(msg => str2err(msg)) */

  private def sequence[A](ls: List[Result[A]]): Result[List[A]] = {
    ls.sequence[Result, A]
  }

  private def runWithState[A](c: Result[A], initial: State, schema: shex.Schema): Either[List[String], A] =
    c.value.run(schema).run(initial)._2

  private val initialState: State = State(Map(), Map(), Map(), 0)

  private def getSchema: Result[shex.Schema] =
    EitherT.liftF(Kleisli.ask[S, shex.Schema])

  private def getState: Result[State] =
    EitherT.liftF(ReaderT.liftF(StateT.get))

  private def setState(s: State): Result[Unit] =
    EitherT.liftF(ReaderT.liftF(StateT.set(s)))

  private def addShapeRefShape(ref: RefNode, shape: shacl.Shape): Result[Unit] =
    EitherT.liftF(ReaderT.liftF(StateT.modify(_.addShapeRefShape(ref, shape))))

  private def getShapesMap: Result[ShapesMap] =
    getState.map(_.shapesMap)

  private def getShapeRef(
      ref: shacl.RefNode
  ): Result[Option[shacl.Shape]] =
    for {
      state <- getState
    } yield state.getShapeRef(ref)

  private def getSchema(schema: shex.Schema): Result[shacl.Schema] =
    for {
      smap <- getShaclShapes(schema)
    } yield shacl.Schema(
      pm = schema.prefixMap,
      imports = List(),
      entailments = List(),
      shapesMap = smap,
      propertyGroups = Map()
    )

  /*  private def cnvPrefixMap(pm: PrefixMap): Map[String, IRI] = {
    pm.pm.map { case (prefix, value) => (prefix.str, value) }
  } */

  private def cnvPath(path: Path): SHACLPath = path match {
    case d: Direct  => PredicatePath(d.pred)
    case i: Inverse => InversePath(PredicatePath(i.pred))
  }

  private def getShaclShapes(
      schema: shex.Schema
  ): Result[Map[shacl.RefNode, shacl.Shape]] = {
    val shexShapes: List[shex.ShapeExpr] = schema.shapes.getOrElse(List())
    for {
      _         <- sequence(shexShapes.map(s => shapeExpr2RefNode(s)))
      shapesMap <- getShapesMap
    } yield shapesMap
  }

  // private type ShapeRefShape = (shacl.RefNode, shacl.Shape)

  private def shapeExpr2RefNode(s: shex.ShapeExpr): Result[shacl.RefNode] = {
    for {
      sm         <- getShapesMap
      _          <- info(s"cnvShapeRefShape: shapeExpr = ${s.toString}\nShapesMap: ${sm}")
      refNode    <- getShapeExprId(s)
      maybeShape <- getShapeRef(refNode)
      _          <- info(s"cnvShapeRefShape: refNode = ${refNode}\nmaybeShape: ${maybeShape}")
      shape <- maybeShape match {
        case Some(s) => ok(s)
        case None =>
          for {
            shape <- shapeExpr2shaclShape(s, refNode.id)
            _     <- addShapeRefShape(refNode, shape)
          } yield shape
      }
    } yield refNode
  }

  private def shapeExpr2shaclShape(
      se: shex.ShapeExpr,
      id: RDFNode
  ): Result[shacl.Shape] =
    se match {
      case s: shex.ShapeAnd => cnvShapeAnd(s, id)
      case s: shex.ShapeOr  => cnvShapeOr(s, id)
      case s: shex.ShapeNot => cnvShapeNot(s, id)
      case nc: shex.NodeConstraint =>
        outCast(cnvNodeConstraint(nc))
      case s: shex.Shape         => cnvShape(s, id)
      case s: shex.ShapeRef      => cnvShapeRef(s, id)
      case s: shex.ShapeExternal => err(s"shapeExpr2shaclShape: Not implemented $s")
      case s: shex.ShapeDecl     => err(s"shapeExpr2shaclShape: Not implemented $s")
    }

  private def cnvShapeAnd(shapeAnd: shex.ShapeAnd, id: RDFNode): Result[shacl.Shape] =
    for {
      shapes <- sequence(shapeAnd.shapeExprs.map(shapeExpr2RefNode))
    } yield {
      shacl.Shape.empty(id).copy(components = List(And(shapes)))
    }

  private def cnvShapeOr(shapeOr: shex.ShapeOr, id: RDFNode): Result[shacl.Shape] =
    for {
      refNodes <- sequence(shapeOr.shapeExprs.map(shapeExpr2RefNode))
    } yield {
      shacl.Shape.empty(id).copy(components = List(Or(refNodes)))
    }

  private def cnvShapeNot(shapeNot: shex.ShapeNot, id: RDFNode): Result[shacl.Shape] =
    for {
      shape <- shapeExpr2RefNode(shapeNot.shapeExpr)
    } yield {
      shacl.Shape.empty(id).copy(components = List(Not(shape)))
    }

  private def cnvLabel(label: shex.ShapeLabel): Result[RefNode] = label match {
    case shex.IRILabel(iri)     => ok(RefNode(iri))
    case shex.BNodeLabel(bnode) => ok(RefNode(bnode))
    case shex.Start             => err(s"Not implemented Start conversion yet")
  }

  private def cnvShapeRef(
      sr: shex.ShapeRef,
      id: RDFNode
  ): Result[shacl.Shape] =
    for {
      refNode <- cnvLabel(sr.reference)
    } yield Shape.empty(id).copy(components = List(NodeComponent(refNode)))

  // TODO: Error handling when virtual, inherit, extra, semActs are defined...
  // TODO: Handle repeated properties
  private def cnvShape(shape: shex.Shape, id: RDFNode): Result[shacl.Shape] =
    getSchema.flatMap(schema =>
      FlatShapeConversion.fromShape(shape, schema) match {
        case Right(flatShape) => {
          info(s"Flat shape: ${flatShape.show}") >>
            cnvFlatShape(flatShape, schema, id)
        }
        case Left(s) => {
          info(s"Not implemented conversion of non-flat schemas") >>
            err(s"""|Not implemented conversion of non-flat shapes yet.
            |Shape: $shape
            |Error: $s
            |""".stripMargin)
        }
      }
    )

  private def cnvFlatShape(
      ns: FlatShapeConversion,
      schema: shex.Schema,
      id: RDFNode
  ): Result[Shape] =
    for {
      p <- sequence(ns.slots.toList.map {
        case (path, constraint) => cnvConstraint(constraint, path, schema, false)
      })
    } yield Shape.empty(id).addPropertyShapes(p.map(_._2))

  /* private def cnvConstraints(cs: Vector[Constraint],
                             path: Path,
                             schema: shex.Schema): Result[List[(shacl.PropertyShape,shacl.RefNode)]] = {
    cs.size match {
      case 0 => ok(List())
      case 1 => for { pair <- cnvConstraint(cs.head, path, schema, false)
                } yield List(pair)
      case _ => cs.toList.map(cnvConstraint(_, path, schema, true)).sequence
    }
  } */

  private def cnvAnnot(a: Annotation): (IRI, RDFNode) =
    (a.predicate, a.obj.getNode)

  private def cnvConstraint(
      c: Constraint,
      path: Path,
      schema: shex.Schema,
      qualified: Boolean
  ): Result[(shacl.PropertyShape, shacl.RefNode)] = {
    val shaclPath = cnvPath(path)
    if (c.hasExtra || qualified) {
      for {
        sref <- getTripleExprId(c.tc)
        shape <- mkQualifiedPropertyShape(
          shaclPath,
          c.shape,
          c.card.min,
          c.card.max,
          sref.id,
          c.as.getOrElse(List()).map(cnvAnnot)
        )
      } yield (shape, sref)
    } else {
      for {
        sref <- getTripleExprId(c.tc)
        shape <- mkPropertyShape(
          shaclPath,
          c.shape,
          c.card.min,
          c.card.max,
          sref.id,
          c.as.getOrElse(List()).map(cnvAnnot)
        )
      } yield (shape, sref)
    }
  }

  /*for {
    ps <- shape.expression match {
      case None => ok(List[(shacl.PropertyShape,shacl.RefNode)]())
      case Some(te) => cnvTripleExpr(te, schema, id)
    }
  } yield {
    Shape.empty(id).copy(propertyShapes = ps.map(_._2))
  }*/

  private def outCast[A, B >: A](r: Result[A]): Result[B] =
    r.map(x => x)

  private def cnvMin(min: Int): Option[Int] =
    if (min == Shacl.defaultMin) None
    else Some(min)

  private def cnvMax(max: shex.Max): Option[Int] =
    max match {
      case shex.Star      => None
      case shex.IntMax(n) => Some(n)
    }

  private def mkPropertyShape(
      path: SHACLPath,
      valueExpr: Option[shex.ShapeExpr],
      min: Int,
      max: shex.Max,
      id: RDFNode,
      annotations: List[(IRI, RDFNode)]
  ): Result[PropertyShape] = {

    val shaclMin: Option[Component]     = cnvMin(min).map(MinCount(_))
    val shaclMax: Option[Component]     = cnvMax(max).map(MaxCount(_))
    val cardComponents: List[Component] = List(shaclMin, shaclMax).flatten

    val rs: Result[PropertyShape] = for {
      components <- valueExpr match {
        case None     => ok(cardComponents)
        case Some(se) => shapeExpr2Components(se).map(_ ++ cardComponents)
      }
      propertyShape = Shape
        .emptyPropertyShape(id, path)
        .copy(
          components = components,
          annotations = annotations
        )
      _ <- addShapeRefShape(RefNode(id), propertyShape)
    } yield propertyShape
    rs
  }

  private def shapeExpr2Components(
      se: shex.ShapeExpr
  ): Result[List[Component]] = se match {
    case sr: shex.ShapeRef =>
      for {
        sref <- cnvLabel(sr.reference)
      } yield List(NodeComponent(sref))
    case nc: NodeConstraint => nodeConstraint2Components(nc)
    case so: shex.ShapeOr =>
      for {
        refs <- sequence(so.shapeExprs.map(shapeExpr2RefNode(_)))
      } yield List(shacl.Or(refs))
    case sa: shex.ShapeAnd =>
      for {
        refs <- sequence(sa.shapeExprs.map(shapeExpr2RefNode(_)))
      } yield List(shacl.And(refs))
    case sn: shex.ShapeNot =>
      for {
        ref <- shapeExpr2RefNode(sn.shapeExpr)
      } yield List(shacl.Not(ref))
    case _ => err(s"shapeExpr2Components: Not implemented yet: ${se}")
  }

  /*  private def shapeExpr2RefNode(se: shex.ShapeExpr): Result[RefNode] =
  for {
    pair <- cnvShapeRefShape(se)
  } yield pair._1 */

  private def mkQualifiedPropertyShape(
      path: SHACLPath,
      valueExpr: Option[shex.ShapeExpr],
      min: Int,
      max: shex.Max,
      id: RDFNode,
      annotations: List[(IRI, RDFNode)]
  ): Result[PropertyShape] = {
    val shaclMin = cnvMin(min)
    val shaclMax = cnvMax(max)
    // val cardComponents = List(shaclMin, shaclMax).flatten
    val rs: Result[PropertyShape] = for {
      components <- valueExpr match {
        case None =>
          for {
            refNode <- getShapeExprId(shex.Shape.empty)
          } yield List[Component](QualifiedValueShape(refNode, shaclMin, shaclMax, None))
        case Some(se) =>
          for {
            sref <- shapeExpr2RefNode(se)
          } yield List[Component](QualifiedValueShape(sref, shaclMin, shaclMax, None))
      }
      ps: PropertyShape = shacl.Shape
        .emptyPropertyShape(id, path)
        .copy(
          components = components,
          annotations = annotations
        )
      _ <- addShapeRefShape(RefNode(id), ps)
    } yield ps
    rs
  }

  private def nodeConstraint2Components(
      nc: NodeConstraint
  ): Result[List[Component]] =
    for {
      nodeKinds <- sequence(nc.nodeKind.map(cnvNodeKind(_)).toList)
      datatypes <- sequence(nc.datatype.map(cnvDatatype(_)).toList)
      xsFacets <- nc.xsFacets.length match {
        case 0 => ok(List[Component]())
        case _ => err[List[Component]](s"Not implemented facets conversion yet\nFacets: ${nc.xsFacets}")
      }
      values <- nc.values match {
        case None => ok(List[In]())
        case Some(vs) =>
          for {
            valueList <- sequence(vs.map(cnvValue(_)))
          } yield List(In(valueList))
      }
    } yield nodeKinds ++ datatypes ++ xsFacets ++ values

  private def cnvNodeConstraint(
      nc: shex.NodeConstraint
  ): Result[shacl.NodeShape] =
    for {
      sref       <- getShapeExprId(nc)
      components <- nodeConstraint2Components(nc)
    } yield shacl.Shape.empty(sref.id).copy(components = components)

  private def getShapeExprsMap: Result[ShapeExprsMap]   = getState.map(_.shapeExprsMap)
  private def getTripleExprsMap: Result[TripleExprsMap] = getState.map(_.tripleExprsMap)

  private def getShapeExprId(
      se: shex.ShapeExpr
  ): Result[RefNode] = se.id match {
    case Some(lbl) =>
      cnvLabel(lbl)
    case None =>
      for {
        shapeExprsMap <- getShapeExprsMap
        id <- shapeExprsMap.get(se) match {
          case Some(sref) => ok(sref)
          case None =>
            for {
              state <- getState
              (newState, ref) = state.newShapeExprRef(se)
              _ <- setState(newState)
            } yield ref
        }
      } yield id
  }

  private def getTripleExprId(te: shex.TripleExpr): Result[RefNode] = te.id match {
    case Some(lbl) => cnvLabel(lbl)
    case None =>
      for {
        tripleExprsMap <- getTripleExprsMap
        id <- tripleExprsMap.get(te) match {
          case Some(sref) => ok(sref)
          case None =>
            for {
              state <- getState
              (newState, ref) = state.newTripleExprRef(te)
              _ <- setState(newState)
            } yield ref
        }
      } yield id
  }

  private def cnvNodeKind(nk: shex.NodeKind): Result[shacl.NodeKind] =
    nk match {
      case shex.IRIKind        => ok(shacl.NodeKind(shacl.IRIKind))
      case shex.BNodeKind      => ok(shacl.NodeKind(shacl.BlankNodeKind))
      case shex.LiteralKind    => ok(shacl.NodeKind(shacl.LiteralKind))
      case shex.NonLiteralKind => ok(shacl.NodeKind(shacl.BlankNodeOrIRI))
    }

  private def cnvDatatype(dt: IRI): Result[shacl.Datatype] =
    ok(Datatype(dt))

  private def cnvValue(v: ValueSetValue): Result[shacl.Value] = v match {
    case shex.IRIValue(iri)           => ok(shacl.IRIValue(iri))
    case shex.StringValue(str)        => ok(shacl.LiteralValue(StringLiteral(str)))
    case shex.DatatypeString(str, dt) => ok(shacl.LiteralValue(DatatypeLiteral(str, dt)))
    case shex.LangString(str, lang)   => ok(shacl.LiteralValue(LangLiteral(str, lang)))
    case _                            => err(s"Not supported conversion of values $v to SHACL yet")
  }

}
