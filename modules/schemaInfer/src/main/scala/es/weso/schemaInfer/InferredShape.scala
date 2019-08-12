package es.weso.schemaInfer
import es.weso.rdf.PREFIXES.{`rdf:langString`, `rdfs:label`, `xsd:string`}
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple
import es.weso.shex._
import SxNamespace._
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

  type ES[A] = Either[String, A]

  def toShapeExpr(eitherLabel: Option[IRI],
                  opts: InferOptions,
                  rdf: RDFReader): Either[String, ShapeExpr] = for {
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
    Shape(shapeLabel,None,None,None,expr,None,None,None)
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

  private def getLabel(iri: IRI,
                       rdf: RDFReader,
                       maybeLang: Option[Lang]
                      ): Either[String, Option[Literal]] = for {
    labels <-
      if (rdf.id == wikidataId) for {
        sourceTriples <- rdf.triplesWithPredicateObject(`wikibase:directClaim`, iri)
        sourceIRIs = sourceTriples.map(_.subj)
        labels <- sourceIRIs.map(rdf.triplesWithSubjectPredicate(_,`rdfs:label`)).toList.sequence[ES,Set[RDFTriple]]
      } yield labels.flatten.map(_.obj)
      else for {
        ts <- rdf.triplesWithSubjectPredicate(iri, `rdfs:label`)
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
