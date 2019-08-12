package es.weso.schemaInfer
import es.weso.rdf.PREFIXES.`xsd:string`
import es.weso.rdf.PREFIXES.`rdf:langString`
import es.weso.rdf.nodes._

sealed abstract trait InferredNodeConstraint {

  def collapse(other: InferredNodeConstraint): InferredNodeConstraint
  def collapseNode(node: RDFNode): InferredNodeConstraint =
    this match {
      case NoConstraint =>
        PlainNode(node)
      case PlainNode(n) =>
        if (node == n) PlainNode(n)
        else collectKind(n, node)
      case InferredIRI =>
        if (node.isIRI) InferredIRI
        else InferredNone
      case InferredBlankNode =>
        if (node.isBNode) InferredBlankNode
        else InferredNone
      case InferredString =>
        node match {
          case s: StringLiteral                 => InferredString
          case DatatypeLiteral(_, `xsd:string`) => InferredString
          case DatatypeLiteral(_, _)            => InferredLiteral
          case _                                => InferredNone
        }
      case InferredLiteral =>
        if (node.isLiteral) InferredLiteral
        else InferredNone
      case InferredDatatype(dt) =>
        node match {
          case l: Literal =>
            if (dt == l.dataType) InferredDatatype(dt)
            else InferredLiteral
          case _ => InferredNone
        }
      case InferredLang(lang) =>
        node match {
          case l: LangLiteral =>
            if (l.lang == lang) InferredLang(lang)
            else InferredLangString
          case l: Literal => InferredLiteral
          case _          => InferredNone
        }
      case InferredLangString =>
        node match {
          case l: LangLiteral => InferredLangString
          case l: Literal     => InferredLiteral
          case _              => InferredNone
        }
      case InferredNone => InferredNone
      case Ref(lbl) => node match {
        case iri: IRI => Ref(lbl)
        case _ => {
          // println(s"Inferred ref($lbl) collapse with $node => None")
          InferredNone
        }
      }
    }

  def collectKind(n1: RDFNode, n2: RDFNode): InferredNodeConstraint = (n1, n2) match {
    case (i1: IRI, i2: IRI)     => InferredIRI
    case (b1: BNode, b2: BNode) => InferredBlankNode
    case (l1: LangLiteral, l2: LangLiteral) =>
      if (l1.lang == l2.lang) InferredLang(l1.lang)
      else InferredLangString
    case (l1: Literal, l2: Literal) =>
      (l1.dataType, l2.dataType) match {
        case (`xsd:string`, `xsd:string`) => InferredString
        case (dt1, dt2) =>
          if (dt1 == dt2) InferredDatatype(dt1)
          else InferredLiteral
      }
    case (_, _) => InferredNone
  }

  /**
    * IRI employed by node constraint
    * @return
    */
  def getIRI: Option[IRI]

}

/**
  * No constraint inferred.
  * This is different from InferredNone which means that it has been inferred that there is no constraint
  */
case object NoConstraint extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other
  override def getIRI = None
}
case class PlainNode(node: RDFNode) extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other.collapseNode(node)
  override def getIRI = node match {
    case iri: IRI => Some(iri)
    case DatatypeLiteral(_,dt) => Some(dt)
    case _ => None
  }
}
case class Ref(lbl: IRI) extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = {
    other match {
      case Ref(otherLbl) => if (lbl == otherLbl) Ref(lbl)
      else {
        // println(s"Inferred references with different labels")
        InferredNone
      }
      case InferredIRI => {
        Ref(lbl)
      }
      case _ => {
        // println(s"Inferred references label $lbl and other node $other")
        InferredNone
      }
    }
  }
  override def getIRI = None
}
case object InferredIRI extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case InferredIRI => InferredIRI
    case _ => InferredNone
  }
  override def getIRI = None
}

case object InferredBlankNode extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case InferredBlankNode => InferredBlankNode
    case _ => InferredNone
  }
  override def getIRI = None
}

case object InferredLiteral extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case InferredLiteral | InferredString | InferredLangString | _:InferredLang | _:InferredDatatype=> InferredLiteral
    case _ => InferredNone
  }
  override def getIRI = None
}

case object InferredString extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case InferredString => InferredString
    case InferredLiteral | InferredLangString | _:InferredLang | _:InferredDatatype=> InferredLiteral
    case _ => InferredNone
  }
  override def getIRI = None
}

case class InferredLang(lang: Lang) extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case l : InferredLang =>
      if (lang.lang == l.lang) InferredLang(lang)
      else InferredLangString
    case InferredLangString => InferredLangString
    case InferredLiteral | _:InferredDatatype=> InferredLiteral
    case _ => InferredNone
  }

  override def getIRI = None
}

case object InferredLangString extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case _ : InferredLang | InferredLangString => InferredLangString
    case InferredString | InferredLiteral | _:InferredDatatype=> InferredLiteral
    case _ => InferredNone
  }

  override def getIRI = Some(`rdf:langString`)
}

case class InferredDatatype(dt: IRI) extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = other match {
    case d : InferredDatatype => if (d.dt == dt) InferredDatatype(dt)
    else InferredLiteral
    case InferredString | InferredLiteral | _:InferredDatatype | InferredLangString | _: InferredLang => InferredLiteral
    case _ => InferredNone
  }
  override def getIRI = Some(dt)
}

case object InferredNone extends InferredNodeConstraint {
  override def collapse(other: InferredNodeConstraint) = InferredNone
  override def getIRI = None
}

