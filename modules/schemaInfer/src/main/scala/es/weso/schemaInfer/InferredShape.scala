package es.weso.schemaInfer
import es.weso.rdf.PREFIXES.{`rdf:langString`, `rdfs:label`, `xsd:string`}
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple
import es.weso.shex._
import SxNamespace._
import fs2.Stream
import cats.effect.IO
import cats.data._
import PossiblePrefixes._

case class InferredShape(nodeShape: InferredNodesValue,
                         smap: Map[IRI, InferredNodesValue]) {

  def get(label: IRI): Option[InferredNodesValue] =
    smap.get(label)

  def updated(label: IRI, shape: InferredNodesValue): InferredShape=
    this.copy(smap = smap.updated(label,shape))

  def collapse(other: InferredShape): InferredShape = {
    val zero = smap
    def cmb(s: Map[IRI, InferredNodesValue],
            current: (IRI, InferredNodesValue)
           ): Map[IRI, InferredNodesValue] = {
      val (iri,iv) = current
      s.get(iri) match {
        case None => s.updated(iri,iv)
        case Some(other) => s.updated(iri, iv.collapse(other))
      }
    }

    this.copy(
      nodeShape = if (other.nodeShape == nodeShape) nodeShape else {
        nodeShape.collapse(other.nodeShape)
      },
      smap = other.smap.foldLeft(zero)(cmb)
    )
  }

  def isEmpty: Boolean = smap.isEmpty

  def values: List[InferredNodesValue] = smap.values.toList

  type ES[A] = EitherT[IO, String, A]

  def toShapeExpr(eitherLabel: Option[IRI],
                  opts: InferOptions,
                  rdf: RDFReader): ES[ShapeExpr] = for {
    es <- smap.toList.sortWith(opts.sortFunction(opts.possiblePrefixMap)).map {
      case (iri, c) => mkTripleExpr(iri,c,rdf,opts)
    }.sequence[ES,TripleExpr]
  } yield {
    val shapeLabel = eitherLabel.map(IRILabel(_))
    val expr: Option[TripleExpr] = es.length match {
      case 0 => None
      case 1 => Some(es.head)
      case _ => Some(EachOf(None, es, None, None, None, None))
    }
    Shape.empty.copy(id = shapeLabel, expression = expr) 
  }

  private def mkTripleExpr(iri: IRI,
                           c: InferredNodesValue,
                           rdf: RDFReader,
                           inferOptions: InferOptions
                          ): ES[TripleExpr] = for {
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

  private def fromIO[A](io: IO[A]): ES[A] = EitherT.liftF(io)
  private def fromStream[A](s: Stream[IO,A]): ES[List[A]] = fromIO(s.compile.toList)

  private def getLabelsFromNode(t: RDFNode): ES[List[RDFNode]] = {
    ???
  }

  private def getLabel(iri: IRI,
                       rdf: RDFReader,
                       maybeLang: Option[Lang]
                      ): EitherT[IO, String, Option[Literal]] = for {
    labels <-
      if (rdf.id == wikidataId) for {
        sourceTriples <- fromStream[RDFTriple](rdf.triplesWithPredicateObject(`wikibase:directClaim`, iri))
        sourceIRIs = sourceTriples.map(_.subj)
        labels <- sourceIRIs.map(getLabelsFromNode).sequence
        // labels <- sourceIRIs.map(fromStream(rdf.triplesWithSubjectPredicate(_,`rdfs:label`))).sequence
      } yield labels.flatten
      else for {
        ts <- fromStream[RDFTriple](rdf.triplesWithSubjectPredicate(iri, `rdfs:label`))
      } yield ts.map(_.obj)
  } yield {
    val okLabels = labels.collect {
      case l: Literal if hasLang(l,maybeLang) => l
    }
    okLabels.headOption
  }

  private def mkShExConstraint(c: InferredNodeConstraint,
                               options: InferOptions): ShapeExpr = c match {
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
    case _@inferredNodeConstraint =>
      sys.error(s"Don't know what to do with this InferredNodeConstraint: $inferredNodeConstraint")
  }

  private def mkMinCardinality(n: Integer): Integer =
    if (n == 1) 1
    else 0

  private def mkMaxCardinality(n: Integer): Max =
    if (n == 1) IntMax(1)
    else Star

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

}

object InferredShape {
  def empty: InferredShape = InferredShape(NoConstraintNodesValue, Map())
}
