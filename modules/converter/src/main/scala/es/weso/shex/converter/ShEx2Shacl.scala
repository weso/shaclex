package es.weso.shex.converter

import cats.Id
import cats.data.{EitherT, StateT}
import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import es.weso.rdf.PrefixMap
import cats.implicits._
import es.weso.shapeMaps.QueryShapeMap
import es.weso.shex.ValueSetValue

object ShEx2Shacl {

  type Err = String
  type ShapesMap = Map[shacl.ShapeRef, shacl.Shape]
  type ShapeExprsMap = Map[shex.ShapeExpr, shacl.ShapeRef]
  type TripleExprsMap = Map[shex.TripleExpr, shacl.ShapeRef]

  case class State(shapesMap: ShapesMap,
                   shapeExprsMap: ShapeExprsMap,
                   tripleExprsMap: TripleExprsMap,
                   counter: Int
                  ) {

    def addShapeRefShape(sref: ShapeRef, s: Shape): State =
      this.copy(shapesMap = shapesMap.updated(sref,s))

    def newShapeExprRef(se: shex.ShapeExpr): (State, shacl.ShapeRef) = {
      val id = ShapeRef(BNode("B" + counter))
      (this.copy(
        shapeExprsMap = shapeExprsMap.updated(se, id),
        counter = counter + 1
      ), id
      )
    }

    def newTripleExprRef(te: shex.TripleExpr): (State, shacl.ShapeRef) = {
      val id = ShapeRef(BNode("B" + counter))
      (this.copy(
        tripleExprsMap = tripleExprsMap.updated(te, id),
        counter = counter + 1
      ), id
      )
    }

  }

  type S[A] = StateT[Id,State,A]
  type Result[A] = EitherT[S,List[Err],A]

  def ok[A](x: A): Result[A] = {
    EitherT.pure(x)
  }

  def err[A](msg: String): Result[A] = {
    EitherT.leftT[S,A](List(msg))
  }

  def sequence[A](ls: List[Result[A]]): Result[List[A]] = {
    ls.sequence[Result,A]
  }

  def runWithState[A](c: Result[A], initial: State): Either[List[String],A] =
    c.value.run(initial)._2

  val initialState : State = State(Map(),Map(),Map(),0)

  def getState: Result[State] =
    EitherT.liftF(StateT.get)

  def setState(s: State): Result[Unit] =
    EitherT.liftF(StateT.set(s))

  def addShapeRefShape(ref: ShapeRef, shape: Shape): Result[Unit] =
    EitherT.liftF(StateT.modify(_.addShapeRefShape(ref,shape)))

  def getShapesMap: Result[ShapesMap] =
    getState.map(_.shapesMap)

  def shex2Shacl(schema: shex.Schema,
                 shapeMap: Option[QueryShapeMap]
                ): Either[List[String], shacl.Schema] = {
   runWithState(getSchema(schema),initialState)
  }

  def getSchema(schema: shex.Schema): Result[shacl.Schema] = for {
   _ <- getShaclShapes(schema)
   smap <- getShapesMap
  } yield shacl.Schema(pm = schema.prefixMap, shapesMap = smap)

  def cnvPrefixMap(pm: PrefixMap): Map[String, IRI] = {
    pm.pm.map { case (prefix, value) => (prefix.str, value) }
  }

  def getShaclShapes(schema: shex.Schema): Result[Map[shacl.ShapeRef, shacl.Shape]] = {
    val shexShapes: List[shex.ShapeExpr] = schema.shapes.getOrElse(List())
    sequence(shexShapes.map(s => cnvShapeRefShape(s,schema))).map(_.toMap)
  }

  type ShapeRefShape = (shacl.ShapeRef, shacl.Shape)

  def cnvShapeRefShape(s: shex.ShapeExpr,
                       schema: shex.Schema
                      ): Result[ShapeRefShape] = {
    for {
      sref    <- getShapeExprId(s)
      shape <- cnvShapeExpr(s, schema, sref.id)
      _ <- addShapeRefShape(sref, shape)
    } yield (sref, shape)
  }

  def cnvShapeExpr(se: shex.ShapeExpr, schema: shex.Schema, id: RDFNode): Result[shacl.Shape] =
    se match {
      case s: shex.ShapeAnd => cnvShapeAnd(s,schema,id)
      case s: shex.ShapeOr => cnvShapeOr(s,schema,id)
      case s: shex.ShapeNot => cnvShapeNot(s,schema,id)
      case nc: shex.NodeConstraint => outCast(cnvNodeConstraint(nc, schema))
      case s: shex.Shape => cnvShape(s, schema, id)
      case s: shex.ShapeRef => err(s"cnvShapeExpr: Not implemented $s")
      case s: shex.ShapeExternal => err(s"cnvShapeExpr: Not implemented $s")
    }

  def cnvShapeAnd(shapeAnd: shex.ShapeAnd, schema: shex.Schema, id: RDFNode): Result[shacl.Shape] =
    for {
    shapes <- sequence(shapeAnd.shapeExprs.map(cnvShapeRefShape(_, schema).map(_._1)))
    } yield {
    shacl.Shape.empty(id).copy(components = List(And(shapes)))
    }

  def cnvShapeOr(shapeOr: shex.ShapeOr,
                 schema: shex.Schema,
                 id: RDFNode
                ): Result[shacl.Shape] =
  for {
    shapes <- sequence(shapeOr.shapeExprs.map(cnvShapeRefShape(_, schema).map(_._1)))
  } yield {
    shacl.Shape.empty(id).copy(components = List(Or(shapes)))
  }

  def cnvShapeNot(shapeNot: shex.ShapeNot,
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
  def cnvShape(shape: shex.Shape,
               schema: shex.Schema,
               id: RDFNode
              ): Result[shacl.Shape] = for {
    ps <- shape.expression match {
      case None => ok(List[(shacl.PropertyShape,shacl.ShapeRef)]())
      case Some(te) => cnvTripleExpr(te, schema, id)
    }
  } yield {
    Shape.empty(id).copy(propertyShapes = ps.map(_._2))
  }

  def outCast[A,B >: A](r:Result[A]): Result[B] = r.map(x => x)

  def cnvTripleExpr(te: shex.TripleExpr, schema: shex.Schema, id: RDFNode): Result[List[(shacl.PropertyShape, shacl.ShapeRef)]] = {
    te match {
      case e: shex.EachOf => err(s"cnvTripleExpr: Not implemented EachOf conversion yet")
      case e: shex.OneOf => err(s"cnvTripleExpr: Not implemented OneOf conversion yet")
      case e: shex.Inclusion => err(s"cnvTripleExpr: Not implemented Inclusion conversion yet")
      case tc: shex.TripleConstraint => cnvTripleConstraint(tc, schema, id).map(List(_))
    }
  }

  def cnvTripleConstraint(tc: shex.TripleConstraint,
                          schema: shex.Schema,
                          id: RDFNode
                         ): Result[(shacl.PropertyShape, shacl.ShapeRef)] = {
    if (tc.negated)
      err(s"cnvTripleConstraint: Not implemented negated")
    else {
      val path = if (!tc.inverse) PredicatePath(tc.predicate)
                 else InversePath(PredicatePath(tc.predicate))
      for {
       sref <- getTripleExprId(tc)
       ps <- mkPropertyShape(
         path,
         tc.valueExpr, tc.min, tc.max, schema, sref.id)
      } yield (ps,sref)
    }
  }

  def mkPropertyShape(path: SHACLPath,
                      valueExpr: Option[shex.ShapeExpr],
                      min: Int,
                      max: shex.Max,
                      schema: shex.Schema,
                      id: RDFNode
                     ): Result[PropertyShape] = {
    val shaclMin: Option[Int] =
      if (min == Shacl.defaultMin) None
      else Some(min)
    val shaclMax: Option[Int] =
      max match {
        case shex.Star => None
        case shex.IntMax(n) => Some(n)
    }
    val rs: Result[PropertyShape] = for {
     components <- valueExpr match {
       case None => ok(List[Component]())
       case Some(se) => cnvShapeRefShape(se,schema).map{
         case (sref,_) => List[Component](QualifiedValueShape(sref, shaclMin, shaclMax, None))
       }
     }
     ps: PropertyShape = Shape.emptyPropertyShape(id, path).copy(
       components = components
     )
     _ <- addShapeRefShape(ShapeRef(id), ps)
    } yield ps
    rs
  }

  def cnvNodeConstraint(
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

  def getShapeExprsMap: Result[ShapeExprsMap] = getState.map(_.shapeExprsMap)
  def getTripleExprsMap: Result[TripleExprsMap] = getState.map(_.tripleExprsMap)

  def getShapeExprId(se: shex.ShapeExpr): Result[ShapeRef] = se.id match {
    case Some(shex.IRILabel(iri)) => ok(ShapeRef(iri))
    case Some(shex.BNodeLabel(bnode)) => ok(ShapeRef(bnode))
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

  def getTripleExprId(te: shex.TripleExpr): Result[ShapeRef] = te.id match {
    case Some(shex.IRILabel(iri)) => ok(ShapeRef(iri))
    case Some(shex.BNodeLabel(bnode)) => ok(ShapeRef(bnode))
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

  def cnvNodeKind(nk: shex.NodeKind): Result[shacl.NodeKind] =
    nk match {
      case shex.IRIKind => ok(shacl.NodeKind(shacl.IRIKind))
      case shex.BNodeKind => ok(shacl.NodeKind(shacl.BlankNodeKind))
      case shex.LiteralKind => ok(shacl.NodeKind(shacl.LiteralKind))
      case shex.NonLiteralKind => ok(shacl.NodeKind(shacl.BlankNodeOrIRI))
    }

  def cnvDatatype(dt: IRI): Result[shacl.Datatype] =
    ok(Datatype(dt))

  def cnvValue(v: ValueSetValue): Result[shacl.Value] = v match {
    case shex.IRIValue(iri) => ok(shacl.IRIValue(iri))
    case shex.StringValue(str) => ok(shacl.LiteralValue(StringLiteral(str)))
    case shex.DatatypeString(str,dt) => ok(shacl.LiteralValue(DatatypeLiteral(str,dt)))
    case shex.LangString(str,lang) => ok(shacl.LiteralValue(LangLiteral(str,lang)))
    case _ => err(s"Not supported conversion of values $v to SHACL yet")
  }
}
