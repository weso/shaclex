package es.weso.shex.converter

import cats._
import cats.data._
import cats.implicits._
import es.weso.shex.implicits.eqShEx._
import cats.data.{EitherT, State, StateT}
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import es.weso.uml.UMLDiagram
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

  private def getUML: Converter[UML] = get.map(_.uml)

  private def setUML(uml: UML): Converter[Unit] = modify(s => s.copy(uml = uml))

  private def get: Converter[StateValue] = {
    val s: State[StateValue,StateValue] = State.get
    EitherT.liftF(s)
  }

  private def newLabel(maybeLbl: Option[ShapeLabel]): Converter[NodeId] = maybeLbl match {
    case Some(lbl) => for {
      uml <- getUML
      (uml1, id) = uml.newLabel(lbl)
      _ <- setUML(uml1)
    } yield id
    case None => err(s"Shape without label")
  }


  def schema2Uml(schema: Schema): Either[String,UML] = {
    val (state, maybe) = cnvSchema(schema).value.run(StateValue(UML.empty,0)).value
    maybe.map(_ => state.uml)
  }

  private def cnvSchema(schema: Schema): Converter[Unit] = {
    schema.shapes match {
      case None => err(s"No shapes in schema")
      case Some(shapes) => {
        def cmb(x: Unit, s: ShapeExpr): Converter[Unit] = for {
          id <- newLabel(s.id)
          cls <- {
            println(s"Before converting: $s")
            cnvShapeExpr(id, s, schema.prefixMap)
          }
          _ <- updateUML(_.addClass(cls))
        } yield (())
        shapes.foldM(())(cmb)
      }
    }
  }

  private def cnvShapeExpr(id: Id, se: ShapeExpr, pm: PrefixMap): Converter[UMLClass] = se match {
   case _: ShapeOr => err(s"Not implemented UML representation of ShapeOr. You can help us suggesting UML diagrams for OR")
   case sa: ShapeAnd => for {
     entries <- cnvListShapeExprEntries(sa.shapeExprs, id, pm)
   } yield {
     val (label,href) = se.id match {
       case None => ("?", None)
       case Some(lbl) => lbl match {
         case i: IRILabel => (pm.qualify(i.iri), Some(i.iri.str))
         case b: BNodeLabel => (b.bnode.id, None)
       }
     }
     UMLClass(id, label, href, entries)
   }
   case sn: ShapeNot => err(s"Not implemented UML representation of Not yet")
   case s: Shape => {
     for {
      entries <- cnvShape(s,id,pm)
     } yield {
     val (label,href) = mkLabel(se.id, pm)
     UMLClass(id,label,href,entries)
   }
   }
   case s: NodeConstraint => for {
     entries <- cnvNodeConstraint(s,pm)
   } yield {
     val (label,href) = mkLabel(s.id, pm)
     UMLClass(id, label, href, entries)
   }
   case s: ShapeExternal => err(s"Not implemented shapeExternal yet")
   case sr: ShapeRef => err(s"Not implemented ShapeRef yet")
 }

  private def mkLabel(maybeLabel: Option[ShapeLabel], pm: PrefixMap):(String,Option[String]) =
    maybeLabel match {
    case None => ("?", None)
    case Some(lbl) => lbl match {
      case i: IRILabel => (pm.qualify(i.iri), Some(i.iri.str))
      case b: BNodeLabel => (b.bnode.id, None)
    }
  }

  private def cnvListShapeExprEntries(se: List[ShapeExpr],id: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] = {
    def cmb(xss: List[List[UMLEntry]], se: ShapeExpr): Converter[List[List[UMLEntry]]] = for {
      fs <- cnvShapeExprEntries(se,id,pm)
    } yield fs ++ xss
    val zero: List[List[UMLEntry]] = List(List())
    se.foldM(zero)(cmb)
  }


  private def cnvShapeExprEntries(se: ShapeExpr, id: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] = se match {
    case sa: ShapeAnd => for {
      ess <- cnvListShapeExprEntries(sa.shapeExprs,id,pm)
    } yield ess
    case so: ShapeOr => err(s"Nested OR not supported yet")
    case sn: ShapeNot => err(s"Nested NOT not supported yet")
    case s: Shape => cnvShape(s,id,pm)
    case nk: NodeConstraint => for {
      vcs <- cnvNodeConstraint(nk,pm)
    } yield vcs
    case s: ShapeExternal => ok(List(List(UML.external)))
    case sr: ShapeRef => err(s"Not implemented shapeRef yet $sr")
  }

  private def cnvShape(s: Shape, id: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] = for {
    closedEntries <- if (s.closed.getOrElse(false)) ok(List(List(UML.umlClosed))) else ok(List())
    // TODO Extra
    // TODO virtual
    // TODO inherit
    exprEntries <- s.expression match {
      case None => ok(List())
      case Some(e) => cnvTripleExpr(e, id, pm)
    }
  } yield mkList(closedEntries, exprEntries)

  def mkLs[A](ls: List[A]*): List[List[A]] = {
    val zero: List[List[A]] = List()
    def cmb(rest: List[List[A]], x: List[A]): List[List[A]] =
      if (x.isEmpty) rest
      else x :: rest
    ls.foldLeft(zero)(cmb)
  }

  def mkList[A](lss: List[List[A]]*): List[List[A]] = {
    val zero: List[List[A]] = List()
    def cmb(rest: List[List[A]], current: List[List[A]]): List[List[A]] =
      if (current == List() || current == List(List())) rest
      else rest ++ current
    lss.foldLeft(zero)(cmb)
  }

  private def cnvNodeConstraint(nc: NodeConstraint, pm: PrefixMap): Converter[List[List[ValueConstraint]]] = for {
    nks <- cnvNodeKind(nc.nodeKind)
    dt <- cnvDatatype(nc.datatype,pm)
    facets <- cnvFacets(nc.xsFacets,pm)
    values <- cnvValues(nc.values,pm)
    // TODO: datatype, facets...
  } yield mkLs(nks,dt,facets,values)

  private def cnvFacets(fs: List[XsFacet], pm:PrefixMap): Converter[List[ValueConstraint]] = {
    val zero: List[ValueConstraint] = List()
    def cmb(next: List[ValueConstraint], c: XsFacet): Converter[List[ValueConstraint]] = for {
      v <- cnvFacet(c,pm)
    } yield v :: next
    fs.foldM(zero)(cmb)
  }

  private def cnvFacet(facet: XsFacet, pm:PrefixMap): Converter[ValueConstraint] = facet match {
    case MinLength(v) => ok(ValueConstraint(s"MinLength $v", None))
    case MaxLength(v) => ok(ValueConstraint(s"MaxLength $v", None))
    case Length(v) => ok(ValueConstraint(s"Length $v", None))
    case Pattern(r,flags) => ok(ValueConstraint(s"/$r/$flags", None))
    case MinInclusive(n) => ok(ValueConstraint(s"MinInclusive $n", None))
    case MaxInclusive(n) => ok(ValueConstraint(s"MaxInclusive $n", None))
    case MinExclusive(n) => ok(ValueConstraint(s"MinExclusive $n", None))
    case MaxExclusive(n) => ok(ValueConstraint(s"MaxExclusive $n", None))
    case TotalDigits(n) => ok(ValueConstraint(s"TotalDigits $n", None))
    case FractionDigits(n) => ok(ValueConstraint(s"FractionDigits $n", None))
  }

  private def cnvValues(vs: Option[List[ValueSetValue]], pm:PrefixMap): Converter[List[ValueConstraint]] =
    vs match {
      case None => ok(List())
      case Some(vs) => // TODO
         ok(List())
  }

  private def cnvDatatype(dt: Option[IRI], pm:PrefixMap): Converter[List[ValueConstraint]] = dt match {
    case None => ok(List())
    case Some(iri) => ok(List(UML.datatype(pm.qualify(iri), iri.str)))
  }

  private def cnvNodeKind(nk: Option[NodeKind]): Converter[List[ValueConstraint]] = nk match {
    case None => ok(List())
    case Some(IRIKind) => ok(List(UML.iriKind))
    case Some(BNodeKind) => ok(List(UML.bnodeKind))
    case Some(LiteralKind) => ok(List(UML.literalKind))
    case Some(NonLiteralKind) => ok(List(UML.nonLiteralKind))
  }

  private def cnvTripleExpr(e: TripleExpr, id: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] = e match {
    case eo: EachOf => cnvEachOf(eo,id,pm)
    case oo: OneOf => err(s"Not supported oneOf yet")
    case i: Inclusion => err(s"Not supported inclusion yet")
    case tc: TripleConstraint => cnvTripleConstraint(tc,id,pm)
  }

  private def cnvEachOf(eo: EachOf, id: NodeId, pm: PrefixMap): Converter[List[List[UMLEntry]]] = {
    val zero: List[List[UMLEntry]] = List()
    def cmb(next: List[List[UMLEntry]], te: TripleExpr): Converter[List[List[UMLEntry]]] = for {
      es <- cnvTripleExpr(te,id,pm)
    } yield es ++ next
    eo.expressions.foldM(zero)(cmb)
  }

  private def cnvTripleConstraint(tc: TripleConstraint,
                                  id: NodeId,
                                  pm: PrefixMap): Converter[List[List[UMLEntry]]] = {
    val card = cnvCard(tc.min, tc.max)
    val label = pm.qualify(tc.predicate)
    val href = tc.predicate.str
    tc.valueExpr match {
      case None => err(s"No value expr")
      case Some(se) => se match {
        case r: ShapeRef => for {
          rid <- newLabel(Some(r.reference))
          _ <- updateUML(_.addLink(UMLLink(id, rid, label, href, card)))
        } yield {
          List()
        }
        case nc: NodeConstraint => for {
          constraints <- cnvNodeConstraint(nc, pm)
        } yield {
          List(List(UMLField(label, Some(href), constraints.flatten, card)))
        }
        case _ if se === Shape.empty => ok(List(List(UMLField(label, Some(href), List(UML.anyConstraint), card))))
        case _ => err(s"Complex shape $se in triple constraint with predicate ${label} not implemented yet")
      }
    }
  }

  private def cnvCard(min: Int, max: Max): UMLCardinality = (min,max) match {
    case (0,es.weso.shex.Star) => UMLDiagram.Star
    case (1,es.weso.shex.Star) => UMLDiagram.Plus
    case (0, es.weso.shex.IntMax(1)) => UMLDiagram.Optional
    case (m, es.weso.shex.Star) => UMLDiagram.Range(m,Unbounded)
    case (m,es.weso.shex.IntMax(n)) => UMLDiagram.Range(m,UMLDiagram.IntMax(n))
  }

  }