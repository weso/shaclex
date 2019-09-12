package es.weso.shex.converter

import cats.Id
import cats.data.{EitherT, StateT}
import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import cats.implicits._
import es.weso.shex.{Shape => ShExShape, _}
import es.weso.shapeMaps.QueryShapeMap
import es.weso.shex.normalized._

object ShEx2Shacl {

  def shex2Shacl(schema: shex.Schema,
                 shapeMap: Option[QueryShapeMap]
                ): Either[List[String], shacl.Schema] = {
    runWithState(getSchema(schema),initialState)
  }

  private type Err = String
  private type ShapesMap = Map[shacl.RefNode, shacl.Shape]
  private type ShapeExprsMap = Map[shex.ShapeExpr, shacl.RefNode]
  private type TripleExprsMap = Map[shex.TripleExpr, shacl.RefNode]

  private case class State(shapesMap: ShapesMap,
                   shapeExprsMap: ShapeExprsMap,
                   tripleExprsMap: TripleExprsMap,
                   counter: Int
                  ) {

    def addShapeRefShape(sref: RefNode, s: shacl.Shape): State =
      this.copy(shapesMap = shapesMap.updated(sref,s))

    def newShapeExprRef(se: shex.ShapeExpr): (State, shacl.RefNode) = {
      val id = RefNode(BNode("B" + counter))
      (this.copy(
        shapeExprsMap = shapeExprsMap.updated(se, id),
        counter = counter + 1
      ), id
      )
    }

    def newTripleExprRef(te: shex.TripleExpr): (State, shacl.RefNode) = {
      val id = RefNode(BNode("B" + counter))
      (this.copy(
        tripleExprsMap = tripleExprsMap.updated(te, id),
        counter = counter + 1
      ), id
      )
    }

  }

  private type S[A] = StateT[Id,State,A]
  private type Result[A] = EitherT[S,List[Err],A]

  private def ok[A](x: A): Result[A] = {
    EitherT.pure(x)
  }

  private def err[A](msg: String): Result[A] = {
    EitherT.leftT[S,A](List(msg))
  }

  private def str2err(str: String): List[Err] = List(str)

  private def fromEither[A](e: Either[List[Err],A]): Result[A] = EitherT.fromEither[S](e)
  private def fromEitherString[A](e: Either[String,A]): Result[A] =
    EitherT.fromEither[S](e).leftMap(msg => str2err(msg))


  private def sequence[A](ls: List[Result[A]]): Result[List[A]] = {
    ls.sequence[Result,A]
  }

  private def runWithState[A](c: Result[A], initial: State): Either[List[String],A] =
    c.value.run(initial)._2

  private val initialState : State = State(Map(),Map(),Map(),0)

  private def getState: Result[State] =
    EitherT.liftF(StateT.get)

  private def setState(s: State): Result[Unit] =
    EitherT.liftF(StateT.set(s))

  private def addShapeRefShape(ref: RefNode, shape: shacl.Shape): Result[Unit] =
    EitherT.liftF(StateT.modify(_.addShapeRefShape(ref,shape)))

  private def getShapesMap: Result[ShapesMap] =
    getState.map(_.shapesMap)


  private def getSchema(schema: shex.Schema): Result[shacl.Schema] = for {
   _ <- getShaclShapes(schema)
   smap <- getShapesMap
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
    case d: Direct => PredicatePath(d.pred)
    case i: Inverse => InversePath(PredicatePath(i.pred))
  }

  private def getShaclShapes(schema: shex.Schema): Result[Map[shacl.RefNode, shacl.Shape]] = {
    val shexShapes: List[shex.ShapeExpr] = schema.shapes.getOrElse(List())
    sequence(shexShapes.map(s => cnvShapeRefShape(s,schema))).map(_.toMap)
  }

  private type ShapeRefShape = (shacl.RefNode, shacl.Shape)

  private def cnvShapeRefShape(s: shex.ShapeExpr,
                               schema: shex.Schema
                              ): Result[ShapeRefShape] = {
    for {
      sref    <- getShapeExprId(s)
      shape <- cnvShapeExpr(s, schema, sref.id)
      _ <- addShapeRefShape(sref, shape)
    } yield (sref, shape)
  }

  private def cnvShapeExpr(se: shex.ShapeExpr,
                           schema: shex.Schema,
                           id: RDFNode
                          ): Result[shacl.Shape] =
    se match {
      case s: shex.ShapeAnd => cnvShapeAnd(s,schema,id)
      case s: shex.ShapeOr => cnvShapeOr(s,schema,id)
      case s: shex.ShapeNot => cnvShapeNot(s,schema,id)
      case nc: shex.NodeConstraint => outCast(cnvNodeConstraint(nc, schema))
      case s: shex.Shape => cnvShape(s, schema, id)
      case s: shex.ShapeRef => err(s"cnvShapeExpr: Not implemented $s")
      case s: shex.ShapeExternal => err(s"cnvShapeExpr: Not implemented $s")
    }

  private def cnvShapeAnd(shapeAnd: shex.ShapeAnd, schema: shex.Schema, id: RDFNode): Result[shacl.Shape] =
    for {
    shapes <- sequence(shapeAnd.shapeExprs.map(cnvShapeRefShape(_, schema).map(_._1)))
    } yield {
    shacl.Shape.empty(id).copy(components = List(And(shapes)))
    }

  private def cnvShapeOr(shapeOr: shex.ShapeOr,
                 schema: shex.Schema,
                 id: RDFNode
                ): Result[shacl.Shape] =
  for {
    shapes <- sequence(shapeOr.shapeExprs.map(cnvShapeRefShape(_, schema).map(_._1)))
  } yield {
    shacl.Shape.empty(id).copy(components = List(Or(shapes)))
  }

  private def cnvShapeNot(shapeNot: shex.ShapeNot,
                  schema: shex.Schema,
                  id: RDFNode
                 ): Result[shacl.Shape] =
    for {
      shape <- cnvShapeRefShape(shapeNot.shapeExpr, schema).map(_._1)
    } yield {
      shacl.Shape.empty(id).copy(components = List(Not(shape)))
    }

  // TODO: Error handling when virtual, inherit, extra, semActs are defined...
  // TODO: Handle repeated properties
  private def cnvShape(shape: shex.Shape,
               schema: shex.Schema,
               id: RDFNode
              ): Result[shacl.Shape] = if (shape.isNormalized(schema)) {
    for {
      normalized <- fromEitherString(shape.normalized(schema))
      shape <- cnvNormalizedShape(normalized, schema, id)
    } yield shape
  } else {
    err(s"""|Not implemented conversion of non-normalized shapes yet.
        |Shape: $shape
        |Error: ${shape.normalized(schema).fold(identity, s => "Is it normalized?")}
        |""".stripMargin)
  }

  private def cnvNormalizedShape(ns: NormalizedShape, schema: shex.Schema, id: RDFNode) : Result[Shape] = for {
    ps <- sequence(ns.slots.toList.map {
      case (path, constraint) => cnvConstraints(constraint, path, schema)
     })
   } yield
    Shape.empty(id).addPropertyShapes(ps.flatten.map(_._2))

  private def cnvConstraints(cs: Vector[Constraint],
                             path: Path,
                             schema: shex.Schema): Result[List[(shacl.PropertyShape,shacl.RefNode)]] = {
    cs.size match {
      case 0 => ok(List())
      case 1 => for { pair <- cnvConstraint(cs.head, path, schema, false)
                } yield List(pair)
      case _ => cs.toList.map(cnvConstraint(_, path, schema, true)).sequence
    }
  }

  private def cnvAnnot(a: Annotation): (IRI, RDFNode) = (a.predicate, a.obj.getNode)

    private def cnvConstraint(c: Constraint,
                              path: Path,
                              schema: shex.Schema,
                              qualified: Boolean): Result[(shacl.PropertyShape,shacl.RefNode)] = {
    val shaclPath = cnvPath(path)
    if (c.hasExtra || qualified) {
      for {
        sref <- getTripleExprId(c.tc)
        shape <- mkQualifiedPropertyShape(shaclPath, c.shape, c.card.min, c.card.max, schema, sref.id, c.as.getOrElse(List()).map(cnvAnnot))
      } yield (shape,sref)
    } else {
      for {
        sref <- getTripleExprId(c.tc)
        shape <- mkPropertyShape(shaclPath, c.shape, c.card.min, c.card.max, schema, sref.id, c.as.getOrElse(List()).map(cnvAnnot))
      } yield (shape,sref)
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

  private def outCast[A,B >: A](r:Result[A]): Result[B] = r.map(x => x)

  private def cnvTripleExpr(te: shex.TripleExpr, schema: shex.Schema, id: RDFNode): Result[List[(shacl.PropertyShape, shacl.RefNode)]] = {
    te match {
      case e: shex.EachOf => err(s"cnvTripleExpr: Not implemented EachOf $e conversion yet")
      case e: shex.OneOf => err(s"cnvTripleExpr: Not implemented OneOf $e conversion yet")
      case e: shex.Inclusion => err(s"cnvTripleExpr: Not implemented $e Inclusion conversion yet")
      case tc: shex.TripleConstraint => cnvTripleConstraint(tc, schema, id).map(List(_))
      case e: shex.Expr => err(s"cnvTripleExpr: Not implemented Expr: $e conversion yet")
    }
  }

  private def cnvTripleConstraint(tc: shex.TripleConstraint,
                          schema: shex.Schema,
                          id: RDFNode
                         ): Result[(shacl.PropertyShape, shacl.RefNode)] = {
    if (tc.negated)
      err(s"cnvTripleConstraint: Not implemented negated")
    else {
      val path = if (!tc.inverse) PredicatePath(tc.predicate)
                 else InversePath(PredicatePath(tc.predicate))
      for {
       sref <- getTripleExprId(tc)
       ps <- mkPropertyShape(
         path,
         tc.valueExpr, tc.min, tc.max, schema, sref.id, tc.annotations.getOrElse(List()).map(cnvAnnot))
      } yield (ps,sref)
    }
  }

  private def cnvMin(min: Int): Option[Int] =
    if (min == Shacl.defaultMin) None
    else Some(min)

  private def cnvMax(max: shex.Max): Option[Int] =
    max match {
      case shex.Star => None
      case shex.IntMax(n) => Some(n)
    }

  private def mkPropertyShape(path: SHACLPath,
                      valueExpr: Option[shex.ShapeExpr],
                      min: Int,
                      max: shex.Max,
                      schema: shex.Schema,
                      id: RDFNode,
                      annotations: List[(IRI,RDFNode)]
                     ): Result[PropertyShape] = {
    val shaclMin: Option[Component] =
      if (min == Shacl.defaultMin) None
      else Some(MinCount(min))

    val shaclMax: Option[Component] =
      max match {
        case shex.Star => None
        case shex.IntMax(n) => Some(MaxCount(n))
    }
    val cardComponents = List(shaclMin, shaclMax).flatten

    val rs: Result[PropertyShape] = for {
     components <- valueExpr match {
       case None => ok(cardComponents)
       case Some(se) => cnvShapeRefShape(se,schema).map{
         case (sref,_) => {
           cardComponents ++ List(NodeComponent(sref))
         }
       }
     }
     ps: PropertyShape = Shape.emptyPropertyShape(id, path).copy(
       components = components,
       annotations = annotations
     )
     _ <- addShapeRefShape(RefNode(id), ps)
    } yield ps
    rs
  }


  private def mkQualifiedPropertyShape(path: SHACLPath,
                              valueExpr: Option[shex.ShapeExpr],
                              min: Int,
                              max: shex.Max,
                              schema: shex.Schema,
                              id: RDFNode,
                              annotations: List[(IRI,RDFNode)]
                             ): Result[PropertyShape] = {
    val shaclMin = cnvMin(min)
    val shaclMax = cnvMax(max)
    val rs: Result[PropertyShape] = for {
      components <- valueExpr match {
        case None => ok(List[Component]())
        case Some(se) => cnvShapeRefShape(se,schema).map{
          case (sref,_) => List[Component](QualifiedValueShape(sref, shaclMin, shaclMax, None))
        }
      }
      ps: PropertyShape = shacl.Shape.emptyPropertyShape(id, path).copy(
        components = components,
        annotations = annotations
      )
      _ <- addShapeRefShape(RefNode(id), ps)
    } yield ps
    rs
  }

  private def cnvNodeConstraint(
    nc: shex.NodeConstraint,
    schema: shex.Schema): Result[shacl.NodeShape] =
   for {
    sref <- getShapeExprId(nc)
    nodeKinds <- sequence(nc.nodeKind.map(cnvNodeKind(_)).toList)
    datatypes <- sequence(nc.datatype.map(cnvDatatype(_)).toList)
    xsFacets <- nc.xsFacets.length match {
      case 0 => ok(List[Component]())
      case _ => err[List[Component]](s"Not implemented facets conversion yet\nFacets: ${nc.xsFacets}")
    }
    values <- nc.values match {
      case None => ok(List[In]())
      case Some(vs) => for {
        valueList <- sequence(vs.map(cnvValue(_)))
      } yield List(In(valueList))
    }
    components = nodeKinds ++ datatypes ++ xsFacets ++ values
   } yield shacl.Shape.empty(sref.id).copy(components = components)

  private def getShapeExprsMap: Result[ShapeExprsMap] = getState.map(_.shapeExprsMap)
  private def getTripleExprsMap: Result[TripleExprsMap] = getState.map(_.tripleExprsMap)

  private def getShapeExprId(se: shex.ShapeExpr): Result[RefNode] = se.id match {
    case Some(shex.IRILabel(iri)) => ok(RefNode(iri))
    case Some(shex.BNodeLabel(bnode)) => ok(RefNode(bnode))
    case Some(shex.Start) => err(s"Unimplemented getShapeExprId of Start yet")
    case None => for {
     shapeExprsMap <- getShapeExprsMap
     id <- shapeExprsMap.get(se) match {
       case Some(sref) => ok(sref)
       case None => for {
         state <- getState
         (newState,ref) = state.newShapeExprRef(se)
         _ <- setState(newState)
       } yield ref
     }
    } yield id
  }

  private def getTripleExprId(te: shex.TripleExpr): Result[RefNode] = te.id match {
    case Some(shex.IRILabel(iri)) => ok(RefNode(iri))
    case Some(shex.BNodeLabel(bnode)) => ok(RefNode(bnode))
    case Some(shex.Start) => err(s"Unimplemented getTripleExprId of Start")
    case None => for {
      tripleExprsMap <- getTripleExprsMap
      id <- tripleExprsMap.get(te) match {
        case Some(sref) => ok(sref)
        case None => for {
          state <- getState
          (newState,ref) = state.newTripleExprRef(te)
          _ <- setState(newState)
        } yield ref
      }
    } yield id
  }

  private def cnvNodeKind(nk: shex.NodeKind): Result[shacl.NodeKind] =
    nk match {
      case shex.IRIKind => ok(shacl.NodeKind(shacl.IRIKind))
      case shex.BNodeKind => ok(shacl.NodeKind(shacl.BlankNodeKind))
      case shex.LiteralKind => ok(shacl.NodeKind(shacl.LiteralKind))
      case shex.NonLiteralKind => ok(shacl.NodeKind(shacl.BlankNodeOrIRI))
    }

  private def cnvDatatype(dt: IRI): Result[shacl.Datatype] =
    ok(Datatype(dt))

  private def cnvValue(v: ValueSetValue): Result[shacl.Value] = v match {
    case shex.IRIValue(iri) => ok(shacl.IRIValue(iri))
    case shex.StringValue(str) => ok(shacl.LiteralValue(StringLiteral(str)))
    case shex.DatatypeString(str,dt) => ok(shacl.LiteralValue(DatatypeLiteral(str,dt)))
    case shex.LangString(str,lang) => ok(shacl.LiteralValue(LangLiteral(str,lang)))
    case _ => err(s"Not supported conversion of values $v to SHACL yet")
  }
}
