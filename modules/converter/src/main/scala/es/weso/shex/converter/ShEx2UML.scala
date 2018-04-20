package es.weso.shex.converter

import cats._
import cats.data._
import cats.implicits._
import cats.data.{EitherT, State, StateT}
import es.weso.rdf.PrefixMap
import es.weso.shex._
import es.weso.uml.UMLDiagram._

object ShEx2UML {

  type Id = Int
  case class StateValue(uml: UML, currentId: Id)
  type S[A] = State[StateValue,A]
  type Converter[A] = EitherT[S,String,A]

  private def ok[A](x:A): Converter[A] =
    EitherT.pure[S, String](x)

  private def err[A](s: String): Converter[A] =
    EitherT.left[A](State.pure(s))

  private def modify(fn: StateValue => StateValue): Converter[Unit] =
    EitherT.liftF(State.modify(fn))

  private def updateUML(fn: UML => UML): Converter[Unit] =
    modify(s => s.copy(uml = fn(s.uml)))

  private def get: Converter[StateValue] = {
    val s: State[StateValue,StateValue] = State.get
    EitherT.liftF(s)
  }


  private def nextId: Converter[Int] = for {
    state <- get
    id = state.currentId
    _ <- modify(s => s.copy(currentId = id + 1))
  } yield id

  def schema2Uml(schema: Schema): Either[String,UML] = {
    val (state, maybe) = cnvSchema(schema).value.run(StateValue(UML.empty,0)).value
    maybe.map(_ => state.uml)
  }

  private def cnvSchema(schema: Schema): Converter[Unit] = {
    schema.shapes match {
      case None => err(s"No shapes in schema")
      case Some(shapes) => {
        def cmb(x: Unit, s: ShapeExpr): Converter[Unit] = for {
          id <- nextId
          cls <- cnvShapeExpr(id, s, schema.prefixMap)
          _ <- updateUML(_.addClass(cls))
        } yield (())
        shapes.foldM(())(cmb)
      }
    }
  }

  private def cnvShapeExpr(id: Id, se: ShapeExpr, pm: PrefixMap): Converter[UMLClass] = se match {
   case _: ShapeOr => err(s"Not implemented UML representation of ShapeOr. You can help us suggesting UML diagrams for OR")
   case sa: ShapeAnd => for {
     entries <- cnvListShapeExprEntries(sa.shapeExprs, pm)
   } yield {
     val cid = "C" + id
     val (label,href) = se.id match {
       case None => ("?", None)
       case Some(lbl) => lbl match {
         case i: IRILabel => (pm.qualify(i.iri), Some(i.iri.str))
         case b: BNodeLabel => (b.bnode.id, None)
       }
     }
     UMLClass(cid, label, href, entries)
   }
   case sn: ShapeNot => err(s"Not implemented UML representation of Not yet")
   case s: Shape => {
     val cid = "C" + id
     for {
      entries <- s.expression match {
       case None => ok(List())
       case Some(e) => cnvTripleExpr(e,cid,pm)
      }
     } yield {
     val (label,href) = se.id match {
       case None => ("?", None)
       case Some(lbl) => lbl match {
         case i: IRILabel => (pm.qualify(i.iri), Some(i.iri.str))
         case b: BNodeLabel => (b.bnode.id, None)
       }
     }
     UMLClass(cid,label,href,entries)
   }
   }
   case s: NodeConstraint => err(s"Not implemented NodeConstraint yet")
   case s: ShapeExternal => err(s"Not implemented shapeExternal yet")
   case sr: ShapeRef => err(s"Not implemented ShapeRef yet")
 }

  private def cnvListShapeExprEntries(se: List[ShapeExpr],pm: PrefixMap): Converter[List[List[UMLEntry]]] = {
    def cmb(xss: List[List[UMLEntry]], se: ShapeExpr): Converter[List[List[UMLEntry]]] = for {
      fs <- cnvShapeExprEntries(se,pm)
    } yield fs :: xss
    val zero: List[List[UMLEntry]] = List(List())
    se.foldM(zero)(cmb)
  }


  private def cnvShapeExprEntries(se: ShapeExpr, pm: PrefixMap): Converter[List[UMLEntry]] = se match {
    case sa: ShapeAnd => err(s"Nested AND not supported yet")
    case so: ShapeOr => err(s"Nested OR not supported yet")
    case sn: ShapeNot => err(s"Nested NOT not supported yet")
    case s: Shape => cnvShape(s,pm)
    case nk: NodeConstraint => cnvNodeConstraint(nk,pm)
    case s: ShapeExternal => ok(List(UML.external))
    case sr: ShapeRef => err(s"Not implemented shapeRef yet $sr")
  }

  private def cnvShape(s: Shape, pm: PrefixMap): Converter[List[UMLEntry]] = ok(List())

  private def cnvNodeConstraint(nc: NodeConstraint, pm: PrefixMap): Converter[List[UMLEntry]] = for {
    nks <- cnvNodeKind(nc.nodeKind)
    // TODO: datatype, facets...
  } yield nks

  private def cnvNodeKind(nk: Option[NodeKind]): Converter[List[UMLEntry]] = nk match {
    case None => ok(List())
    case Some(IRIKind) => ok(List(UML.iriKind))
    case Some(BNodeKind) => ok(List(UML.bnodeKind))
    case Some(LiteralKind) => ok(List(UML.literalKind))
    case Some(NonLiteralKind) => ok(List(UML.nonLiteralKind))
  }

  private def cnvTripleExpr(e: TripleExpr, c: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] =
    ok(List())

}