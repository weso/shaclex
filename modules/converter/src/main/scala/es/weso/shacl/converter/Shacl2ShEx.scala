package es.weso.shacl.converter

import cats.Id
import cats.data._
import cats.syntax.all._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.rdf.path.{InversePath, PredicatePath, SHACLPath}
import es.weso.shex.implicits.showShEx._
import es.weso.shex.linter.ShExLinter
import es.weso.{shacl, _}

object Shacl2ShEx {

  def shacl2ShEx(schema: shacl.Schema): Either[String, (shex.Schema, shapeMaps.QueryShapeMap)] = {
    val (state, eitherSchema) = cnvSchema(schema).value.run(initialState)
    val e = for {
     schema <- eitherSchema
     schema1 = schema.addTripleExprMap(state.tripleExprMap)
     queryMap <- cnvShapeMap(schema)
     lintedSchema <- ShExLinter.inlineInclusions(schema1)
    } yield (lintedSchema,queryMap)
    // println(s"Result of conversion: \n$e")
    e
  }

  def cnvShapeMap(schema: shex.Schema): Either[String,shapeMaps.QueryShapeMap] = Right(shapeMaps.QueryShapeMap(List(), PrefixMap.empty, PrefixMap.empty))

  case class State(tripleExprMap: TEMap) {
    def addLabelTripleExpr(lbl: shex.ShapeLabel, te: shex.TripleExpr): State = {
      this.copy(tripleExprMap = tripleExprMap.updated(lbl,te))
    }
  }

  lazy val initialState: State = State(Map())

  type TEMap = Map[shex.ShapeLabel, shex.TripleExpr]

  type S[A] = StateT[Id,State,A]

  private type Result[A] = EitherT[S,String,A]

  private def ok[A](x:A): Result[A] =
    EitherT.pure(x)

  private def err[A](msg: String): Result[A] = {
    EitherT.leftT[S,A](msg)
  }

  private def modify(fn: State => State): Result[Unit] =
    EitherT.liftF(StateT.modify(fn))

  private def sequence[A](rs: List[Result[A]]): Result[List[A]] = rs.sequence[Result,A]

  private def cnvSchema(schema: shacl.Schema): Result[shex.Schema] = {
    sequence(
      schema.shapes.map(cnvShape(_, schema)
    ).toList).map(
      m => shex.Schema.empty.copy(
        prefixes = Some(schema.pm),
        shapes = Some(m)
      )
    )
  }

  private def cnvShape(c: shacl.Shape,
                       schema: shacl.Schema
                      ): Result[shex.ShapeExpr] =
    c match {
      case nc: shacl.NodeShape => cnvNodeShape(nc, schema)
      case ps: shacl.PropertyShape => for {
        id <- cnvId(c.id)
        te <- cnvPropertyShape(ps)
      } yield shex.Shape.empty.copy(expression = Some(te)).addId(id)
      case s => err(s"cnvShape: Unimplemented conversion of $s")
    }

//  private type ShExId = Option[shex.ShapeLabel]

  private def cnvId(node: RDFNode): Result[shex.ShapeLabel] =
    shex.ShapeLabel.fromRDFNode(node).fold(e => err(e), lbl => ok(lbl))

/*  private def cnvComponents(cs: List[shacl.Component]): Result[List[shex.ShapeExpr]] =
    sequence(cs.map(cnvComponent(_))) */

  private def cnvPropertyShapes(ps: List[shacl.RefNode],
                                schema: shacl.Schema
                               ): Result[List[shex.ShapeExpr]] =
    sequence(ps.map(cnvPropertyShapeRef(_, schema)))

  private def cnvNodeShape(ns: shacl.NodeShape,
                           schema: shacl.Schema
                          ): Result[shex.ShapeExpr] =
    for {
      id <- cnvId(ns.id)
      maybeSe <- cnvComponentsAsShapeExpr(ns.components)
      // _ <- { println(s"MaybeSe: $maybeSe"); ok(()) }
      ps <- cnvPropertyShapes(ns.propertyShapes.toList, schema)
      // _ <- { println(s"ps: $ps"); ok(()) }
      se = maybeSe.getOrElse(shex.ShapeExpr.any)
      se1 = ps.length match {
          case 0 => // TODO: Check when there are more triple exprs...
           se
          case 1 => {
            // println(s"Length 1...")
            ps.head // addExpression(se, ps.head)
          }
          case n if (n > 1) =>
           // addExpression(se, shex.EachOf(None,ps,None,None,None,None))
          shex.ShapeAnd(None, ps,None,None)
        }
      se2 <- addId(se1,id)
    } yield se2

  private def addId(se: shex.ShapeExpr, id: shex.ShapeLabel): Result[shex.ShapeExpr] = {
    ok(se.addId(id))
  }

/*  private def addExpression(se: shex.ShapeExpr, te: shex.TripleExpr): Result[shex.ShapeExpr] = se match {
    case s: shex.Shape => ok(s.copy(expression = Some(te)))
    case nc: shex.NodeConstraint => ok(shex.ShapeAnd(None,
      List(nc,
           shex.Shape.empty.copy(expression=Some(te))))
    )
  }*/

  private def addLabelTripleExpr(lbl: shex.ShapeLabel,
                                 te: shex.TripleExpr
                                ): Result[Unit] =
    for {
     _ <- modify(_.addLabelTripleExpr(lbl,te))
    } yield (())

  private def cnvPropertyShapeRef(sref: shacl.RefNode,
                                  schema: shacl.Schema
                                 ): Result[shex.ShapeExpr] =
    for {
     shape <- getShape(sref, schema)
     se <- shape match {
       case ps: shacl.PropertyShape => for {
         id <- cnvId(ps.id)
         s <- cnvPropertyShape(ps)
         _ <- addLabelTripleExpr(id,s)
       } yield shapeInclusion(id)
       case _ =>
         err[shex.ShapeExpr](s"cnvPropertyShapeRef: reference $sref does not point to a property shape. Shape: $shape")
     }
    } yield se

  private def cnvPropertyShape(ps: shacl.PropertyShape): Result[shex.TripleExpr] =
   for {
    predicateInverse <- getPredicateInversePair(ps.path)
    se <- cnvComponentsAsShapeExpr(ps.components)
    min <- getMinComponent(ps.components)
    max <- getMaxComponent(ps.components)
   } yield shex.TripleConstraint(None,predicateInverse.inverse, None, predicateInverse.pred, se, min, max, None, None, None)

  case class PredicateInverse(pred: IRI, inverse: Option[Boolean])

  private def getMinComponent(components: List[shacl.Component]): Result[Option[Int]] = {
    ok(components.collect { case shacl.MinCount(m) => m }.headOption)
  }

  private def getMaxComponent(components: List[shacl.Component]): Result[Option[shex.Max]] = {
    ok(components.collect { case shacl.MaxCount(m) => shex.IntMax(m) }.headOption)
  }

  // TODO: Conversion of components like BlankNodeOrIRI is ignored by now
  private def cnvComponentsAsShapeExpr(cs: List[shacl.Component]): Result[Option[shex.ShapeExpr]] = for {
    maybeNodeConstraint <- getNodeConstraint(cs)
  } yield maybeNodeConstraint

  private def getNodeConstraint(cs: List[shacl.Component]): Result[Option[shex.NodeConstraint]] = for {
    nk <- getNodeKind(cs)
    dt <- getDatatype(cs)
    valueSet <- getValueSet(cs)
  } yield if (nk.isDefined || dt.isDefined || valueSet.isDefined)
    shex.NodeConstraint(id = None, nodeKind = nk, datatype = dt, xsFacets = List(), values = valueSet, annotations = None, actions = None).some
  else
    none[shex.NodeConstraint]

/*  private def cnvComponentsAsShapeExpr(cs: List[shacl.Component]): Result[Option[shex.ShapeExpr]] = for {
    components <- cnvComponents(cs)
  } yield components length match {
    case 1 => Some(components.head)
    case n if (n > 1) => Some(shex.ShapeAnd(None,components.reverse, None,None))
    case 0 => None
  } */

  private def getNodeKind(cs: List[shacl.Component]): Result[Option[shex.NodeKind]] = {
    val nks: List[shex.NodeKind] = cs.flatMap(component2NodeKind)
    nks.length match {
      case 1 => ok(Some(nks.head))
      case 0 => ok(None)
      case _ => err(s"More than one component provides NodeKind constraints: ${nks.map(_.show).mkString(",")}")
    }
  }

  private def getDatatype(cs: List[shacl.Component]): Result[Option[IRI]] = {
    val nks: List[IRI] = cs.flatMap(component2Datatype)
    nks.length match {
      case 1 => ok(Some(nks.head))
      case 0 => ok(None)
      case _ => err(s"More than one component provides Datatype constraints: ${nks.map(_.show).mkString(",")}")
    }
  }

  private def getValueSet(cs: List[shacl.Component]): Result[Option[List[shex.ValueSetValue]]] = {
    val vss: List[List[shex.ValueSetValue]] = cs.flatMap(component2ListValues)
    vss.length match {
      case 1 => ok(Some(vss.head))
      case 0 => ok(None)
      case _ => err(s"More than one component provides ValueSet constraints: ${vss.map(_.show).mkString(",")}")
    }
  }

  private def component2NodeKind(c: shacl.Component): Option[shex.NodeKind] = c match {
    case nk: shacl.NodeKind => nk.value match {
      case shacl.IRIKind => Some(shex.IRIKind)
      case shacl.BlankNodeKind => Some(shex.BNodeKind)
      case shacl.LiteralKind => Some(shex.LiteralKind)
      case _ => None // TODO: Here we are ignoring the SHACl constraints BlankNodeOrIRI, BlankNodeOrLiteral and IRIOrLiteral
      // The conversion of those constraints should be done before...
    }
    case _ => None
  }

  private def component2Datatype(c: shacl.Component): Option[IRI] = c match {
    case dt: shacl.Datatype => Some(dt.value)
    case _ => None
  }

  private def component2ListValues(c: shacl.Component): Option[List[shex.ValueSetValue]] = c match {
    case cin: shacl.In => Some(cnvIn(cin))
    case _ => None
  }

  def getPredicateInversePair(path: SHACLPath): Result[PredicateInverse] = path match {
    case PredicatePath(iri) => ok(PredicateInverse(iri,None))
    case InversePath(PredicatePath(iri)) => ok(PredicateInverse(iri,Some(true)))
    case _ => err(s"Not supported complex paths in SHACL->ShEx conversion yet: path: ${path}")
  }
/*    val rs : Result[List[shex.ShapeExpr]] = sequence(c.components.map(cnvComponent(_)))
    def next(se: List[shex.ShapeExpr]): Result[shex.ShapeExpr] = se.size match {
      case 1 => {
        shex.ShapeLabel
          .fromRDFNode(c.id)
          .fold(
            e => err(e),
            lbl => ok {
              se.head.addId(lbl)
            }
          )
      }
      case n if (n > 1) => {
        shex.ShapeLabel.fromRDFNode(c.id).fold(
          e => err(e),
          lbl => ok(shex.ShapeAnd(Some(lbl), se))
        )
      }
      case v => err(s"cnvShapeNode, size of components $v not supported")
    }
    rs andThen next */


  /*private def cnvComponent(c: shacl.Component): Result[shex.ShapeExpr] = {
    c match {
      case nk: shacl.NodeKind => cnvNodeKind(nk)
      case dt: shacl.Datatype => cnvDatatype(dt)
      case in: shacl.In => cnvIn(in)
      case _ => err(s"cnvComponent: Unimplemented $c")
    }
  }*/

//  private def shexIri(): shex.ShapeExpr = shex.NodeConstraint.nodeKind(shex.IRIKind, List())
//  private def shexBNode(): shex.ShapeExpr = shex.NodeConstraint.nodeKind(shex.BNodeKind, List())
//  private def shexLiteral(): shex.ShapeExpr = shex.NodeConstraint.nodeKind(shex.LiteralKind, List())
  private def shapeInclusion(lbl: shex.ShapeLabel): shex.ShapeExpr =
    shex.Shape.empty.copy(expression = Some(shex.Inclusion(lbl)))

/*  private def cnvNodeKind(nk: shacl.NodeKind): Result[shex.ShapeExpr] =
   nk.value match {
    case shacl.IRIKind => ok(shexIri)
    case shacl.BlankNodeKind => ok(shexBNode)
    case shacl.LiteralKind => ok(shexLiteral)
    case shacl.BlankNodeOrIRI => ok(shex.ShapeOr.fromShapeExprs(List(shexBNode,shexIri)))
    case shacl.BlankNodeOrLiteral => ok(shex.ShapeOr.fromShapeExprs(List(shexBNode,shexLiteral)))
    case shacl.IRIOrLiteral => ok(shex.ShapeOr.fromShapeExprs(List(shexIri,shexLiteral)))
  } */

/*  private def cnvDatatype(dt: shacl.Datatype): Result[shex.ShapeExpr] = {
   ok(shex.NodeConstraint.datatype(dt.value,List()))
  } */

  private def cnvIn(dt: shacl.In): List[shex.ValueSetValue] =
    dt.list.map(cnvValue)

  private def cnvValue(v: shacl.Value): shex.ValueSetValue =
   v match {
     case shacl.IRIValue(iri) => shex.IRIValue(iri)
     case shacl.LiteralValue(lit) => lit match {
       case StringLiteral(str) => shex.StringValue(str)
       case DatatypeLiteral(str,dt) => shex.DatatypeString(str,dt)
       case LangLiteral(str,lang) => shex.LangString(str,lang)
       case _ => shex.DatatypeString(lit.getLexicalForm,lit.dataType)
     }
  }

  private def getShape(sref: shacl.RefNode,
                       schema: shacl.Schema
                      ): Result[shacl.Shape] = {
    fromEither(schema.shape(sref.id))
  }

  private def fromEither[A](e: Either[String,A]): Result[A] =
    EitherT.fromEither(e)

/*  private def mkIRILabel(iri: IRI): shex.ShapeLabel =
    shex.IRILabel(iri)

  private def mkBNodeLabel(n: Int): shex.ShapeLabel =
    shex.BNodeLabel(BNode(n.toString))
*/
}