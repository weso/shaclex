package es.weso.shex.converter

import cats.implicits._
import es.weso.shex.implicits.eqShEx._
import cats.data.{EitherT, State}
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.PREFIXES._
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

  private def generateId: Converter[Id] = for {
    s <- get
    _ <- modify(_ => s.copy(currentId = s.currentId + 1))
  } yield s.currentId

  private def get: Converter[StateValue] = {
    val s: State[StateValue,StateValue] = State.get
    EitherT.liftF(s)
  }

  private def cnvList[A,B](vs: List[A], cnv: A => Converter[B]): Converter[List[B]] =
    vs.map(cnv(_)).sequence

  private def newLabel(maybeLbl: Option[ShapeLabel]): Converter[NodeId] =
    maybeLbl match {
    case Some(lbl) => for {
      uml <- getUML
      (uml1, id) = uml.newLabel(lbl)
      _ <- setUML(uml1)
    } yield id
    case None => for {
      uml <- getUML
      newId <- generateId
      (uml1,id) = uml.newLabel(BNodeLabel(BNode("L" + newId)))
      _ <- setUML(uml1)
    } yield id
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
         case i: IRILabel => (iri2Label(i.iri,pm), Some(i.iri.str))
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
      case i: IRILabel => (iri2Label(i.iri,pm), Some(i.iri.str))
      case b: BNodeLabel => (b.bnode.id, None)
    }
  }

  private def iri2Label(iri: IRI, pm: PrefixMap): String = {
    // It changes <uri> by [uri] to avoid problems visualizing SVG in HTML
   val ltgt = "<(.*)>".r
   pm.qualify(iri) match {
     case ltgt(s) => s"$s"
     case s => s
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


  private def cnvNodeConstraint(nc: NodeConstraint, pm: PrefixMap): Converter[List[List[ValueConstraint]]] = for {
    nks <- cnvNodeKind(nc.nodeKind)
    dt <- cnvDatatype(nc.datatype,pm)
    facets <- cnvFacets(nc.xsFacets,pm)
    values <- cnvValues(nc.values,pm)
    // TODO: datatype, facets...
  } yield mkLs(nks,dt,facets,values)

  private def cnvShapeOrInline(es: List[ShapeExpr], pm: PrefixMap): Converter[List[List[ValueConstraint]]] =
    if (allReferences(es)) for {
      values <- cnvList(es, cnvShapeRef(pm))
    } yield List(List(ValueExpr("OR", values)))
    else err(s"Complex OR not implemented yet: $es in ShapeOrInline")


  private def cnvFacets(fs: List[XsFacet], pm:PrefixMap): Converter[List[ValueConstraint]] = {
    val zero: List[ValueConstraint] = List()
    def cmb(next: List[ValueConstraint], c: XsFacet): Converter[List[ValueConstraint]] = for {
      v <- cnvFacet(c,pm)
    } yield v :: next
    fs.foldM(zero)(cmb)
  }

  private def cnvFacet(facet: XsFacet, pm:PrefixMap): Converter[ValueConstraint] = facet match {
    case MinLength(v) => ok(Constant(s"MinLength $v"))
    case MaxLength(v) => ok(Constant(s"MaxLength $v"))
    case Length(v) => ok(Constant(s"Length $v"))
    case Pattern(r,flags) => ok(Constant(s"/$r/$flags"))
    case MinInclusive(n) => ok(Constant(s"MinInclusive $n"))
    case MaxInclusive(n) => ok(Constant(s"MaxInclusive $n"))
    case MinExclusive(n) => ok(Constant(s"MinExclusive $n"))
    case MaxExclusive(n) => ok(Constant(s"MaxExclusive $n"))
    case TotalDigits(n) => ok(Constant(s"TotalDigits $n"))
    case FractionDigits(n) => ok(Constant(s"FractionDigits $n"))
  }

  private def cnvValues(vs: Option[List[ValueSetValue]], pm:PrefixMap): Converter[List[ValueConstraint]] =
    vs match {
      case None => ok(List())
      case Some(vs) => for {
        values <- cnvList(vs, cnvValue(_: ValueSetValue, pm))
      } yield List(ValueSet(values))
  }

  private def cnvValue(v: ValueSetValue, pm: PrefixMap): Converter[Value] = v match {
    case IRIValue(iri) => ok(Value(iri2Label(iri,pm),Some(iri.str)))
    case StringValue(str) => ok(Value(str,None))
    case DatatypeString(s,iri) => ok(Value("\"" + s + "\"^^" + iri2Label(iri,pm), None))
    case LangString(s,lang) => ok(Value("\"" + s + "\"@" + lang.lang,None))
    case IRIStem(stem) => ok(Value(s"${iri2Label(stem,pm)}~",None))
    case IRIStemRange(stem,excs) => err(s"Not implemented UML visualization of IRIStemRange($stem,$excs) yet")
    case LanguageStem(lang) => ok(Value(s"@${lang.lang}~",None))
    case LanguageStemRange(lang,excs) => err(s"Not implemented UML visualization of LanguageStemRange($lang,$excs) yet")
    case LiteralStem(stem) => ok(Value("\"" + "\"" + stem + "\"~",None))
    case LiteralStemRange(stem,excs) => err(s"Not implemented UML visualization of LiteralStemRange($stem,$excs) yet")
    case Language(lang) => ok(Value(s"@${lang.lang}",None))
  }

  private def cnvDatatype(dt: Option[IRI], pm:PrefixMap): Converter[List[ValueConstraint]] = dt match {
    case None => ok(List())
    case Some(iri) => ok(List(UML.datatype(iri2Label(iri,pm), iri.str)))
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
    val (label,href) = predicate2lbl(tc.predicate,pm)
    tc.valueExpr match {
      case None => err(s"No value expr for triple constraint $tc")
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
        case _ if se === Shape.empty =>
          ok(List(List(UMLField(label, Some(href), List(UML.anyConstraint), card))))

        case s: Shape => for {
            sid <- newLabel(s.id)
            entries <- cnvShape(s, sid, pm)
            (labelShape,hrefShape) = mkLabel(s.id, pm)
            cls = UMLClass(sid,labelShape,hrefShape,entries)
            _ <- updateUML(_.addClass(cls))
            _ <- updateUML(_.addLink(UMLLink(id,sid,label,href,card)))
        } yield
          List()
        case so: ShapeOr => for {
          vso <- cnvShapeOrInline(so.shapeExprs,pm)
        } yield List(List(UMLField(label, Some(href), vso.flatten, card)))



        case _ => err(s"Complex shape $se in triple constraint with predicate ${label} not implemented yet")
      }
    }
  }

  private def cnvShapeRef(pm: PrefixMap)(sr: ShapeExpr): Converter[ValueConstraint] = sr match {
    case sr:ShapeRef => ok(DatatypeConstraint(iri2Label(sr.reference.toRDFNode.toIRI,pm),sr.reference.toRDFNode.toIRI.str))
    case _ => err(s"cnvShapeRef. Expected shapeRef but found $sr")
  }

  private def allReferences(es: List[ShapeExpr]): Boolean =
    es.forall(isReference(_))

  private def isReference(e: ShapeExpr): Boolean = e match {
    case ShapeRef(r) => true
    case _ => false
  }

  private def cnvCard(min: Int, max: Max): UMLCardinality = (min,max) match {
    case (0,es.weso.shex.Star) => UMLDiagram.Star
    case (1,es.weso.shex.Star) => UMLDiagram.Plus
    case (0, es.weso.shex.IntMax(1)) => UMLDiagram.Optional
    case (m, es.weso.shex.Star) => UMLDiagram.Range(m,Unbounded)
    case (m,es.weso.shex.IntMax(n)) => UMLDiagram.Range(m,UMLDiagram.IntMax(n))
  }

  private def predicate2lbl(iri: IRI, pm: PrefixMap): (Name, HRef) = iri match {
    case `rdf_type` => ("a",iri.str)
    case _ => (iri2Label(iri,pm), iri.str)
  }

  private def mkLs[A](ls: List[A]*): List[List[A]] = {
    val zero: List[List[A]] = List()
    def cmb(rest: List[List[A]], x: List[A]): List[List[A]] =
      if (x.isEmpty) rest
      else x :: rest
    ls.foldLeft(zero)(cmb)
  }

  private def mkList[A](lss: List[List[A]]*): List[List[A]] = {
    val zero: List[List[A]] = List()
    def cmb(rest: List[List[A]], current: List[List[A]]): List[List[A]] =
      if (current == List() || current == List(List())) rest
      else rest ++ current
    lss.foldLeft(zero)(cmb)
  }

}