package es.weso.shex.validator
import cats._
import com.typesafe.scalalogging.LazyLogging
import implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex.ShExError._
import es.weso.shex._
import es.weso.shex.validator.ShExChecker._

case class ValueChecker(schema: Schema)
  extends ShowValidator(schema) with LazyLogging {

  def checkValue(
    attempt: Attempt,
    node: RDFNode)(value: ValueSetValue): CheckTyping = {
    logger.debug(s"checkValue: $node $value")
    valueChecker(node,value).fold(
      e => checkCond(false,attempt,msgErr(e), ""),
      msg => checkCond(true,attempt,msgErr(""), msg))
  }

  /** Checks if a node matchs a valueSet value
    *
    * @param node: RDF node
    * @param value: ValueSetValue
    * @return Left(msg) if node doesn't match, msg constains some explanation, Right(()) if it matches
    */
  private[validator] def valueChecker(node: RDFNode,
                           value: ValueSetValue): Either[String, String] = {
    value match {
      case IRIValue(iri) =>
        node match {
        case i: IRI => if (iri == i) Right(s"${iri.show} is equal to ${i.show}")
          else Left(s"${iri.show} != ${i.show}")
        case _ => Left(s"${node.show} != ${iri.show}")
      }
      case StringValue(s) =>
        node match {
        case l: Literal => if(s == l.getLexicalForm && l.dataType == `xsd:string`)
          Right(s"${node.show} == ${l}")
        else
          Left(s"${node.show} != ${l}")
        case _ => Left(s"${node.show} != ${value}")
      }
      case DatatypeString(s, iri) => {
        node match {
          case l: Literal =>
            if (s == l.getLexicalForm && iri == l.dataType) Right(s"${node.show} == ${l}")
            else Left(s"${node.show} != ${l}")
          case _ => Left(s"${node.show} != ${value}")
        }
      }
      case LangString(s, lang) => {
        node match {
          case LangLiteral(str, l) =>
            if (s == str && lang == l)
              Right(s"${node.show} == ${value}")
            else
              Left(s"${node.show} != ${value}")
          case _ => Left(s"${node.show} != ${value}")
        }
      }
      case LanguageStem(stem) => node match {
          case l: LangLiteral => {
            if (checkLanguageStem(l.lang, stem)) Right(s"${node.show} lang(${l.lang}) matches ${stem}")
            else Left(s"${node.show} lang(${l.lang}) does not match ${stem}")
          }
          case _ => Left(s"${node.show} is not a language tagged literal")
        }
      case ls @ LanguageStemRange(stem, excls) => node match {
          case LangLiteral(x, lang) => {
            if (checkLangStemRange(lang, stem, excls))
              Right(s"${node.show} lang($lang) matches ${ls}")
            else
              Left(s"${node.show} lang($lang) does not match ${ls}")
          }
          case _ => Left(s"${node.show} is not a language tagged literal")
        }
      case Language(langTag) => node match {
          case LangLiteral(x, Lang(lang)) =>
            if (langTag.lang.equalsIgnoreCase(lang))
              Right(s"${node.show} lang($lang) matches ${langTag}")
            else Left(s"${node.show} lang($lang) does not match ${langTag}")
          case _ => Left(s"${node.show} is not a language tagged literal")
        }
      case IRIStem(stem) => node match {
        case i: IRI =>
          if (i.getLexicalForm.startsWith(stem.getLexicalForm))
            Right(s"${node.show} matches with stem ${stem.show}")
          else
            Left(s"${node.show} does not match stem ${stem.show}")
        case _ => Left(s"${node.show} must be an IRI to match with IRI stem ${stem.show}")
      }
      case is @ IRIStemRange(stem, excls) => node match {
          case i: IRI => if (checkIRIStemRange(i, stem, excls))
            Right(s"${node.show} matches IRI stem range $is")
          else
            Left(s"${node.show} does not match IRI stem range $is")
          case _ => Left(s"${node.show} is not an IRI")
        }
      case LiteralStem(stem) => node match {
        case l: Literal =>
          if (l.getLexicalForm.startsWith(stem))
            Right(s"${node.show} matches with stem ${stem.show}")
          else
            Left(s"${node.show} does not match stem ${stem.show}")
        case _ => Left(s"${node.show} must be a Literal to match with Literal stem ${stem.show}")
      }
      case ls @ LiteralStemRange(stem, excls) => node match {
          case l: Literal => {
            if (checkLiteralStemRange(l, stem, excls)) Right(s"${node.show} matches Literal stem range $ls")
            else Left(s"${node.show} does not match Literal stem range $ls")
          }
          case _ => Left(s"${node.show} is not a Literal")
        }
   }
  }

  private def checkLanguageStem(l: Lang, stem: Lang): Boolean = {
    val strL = l.lang.toLowerCase
    val strStem = stem.lang.toLowerCase
    if (strStem.isEmpty) true else {
    strL === strStem || strL.startsWith(strStem + "-")
   }
  }

  private def checkIRIStemRange(iri: IRI, stemRange: IRIStemRangeValue, excls: Option[List[IRIExclusion]]): Boolean = {
    val cond1 = stemRange match {
      case IRIStemWildcard() => true
      case IRIStemValueIRI(iriStem) => iri.getLexicalForm.startsWith(iriStem.getLexicalForm)
    }
    val cond2 = excls match {
      case None => true
      case Some(es) => es.forall(e => checkIRIExclusion(iri,e))
    }
    cond1 && cond2
  }

  private def checkLiteralStemRange(lit: Literal, stemRange: LiteralStemRangeValue, excls: Option[List[LiteralExclusion]]): Boolean = {
    val cond1 = stemRange match {
      case LiteralStemRangeWildcard() => true
      case LiteralStemRangeString(litStem) => lit.getLexicalForm.startsWith(litStem)
    }
    val cond2 = excls match {
      case None => true
      case Some(es) => es.forall(e => checkLiteralExclusion(lit,e))
    }
    cond1 && cond2
  }

  private def checkLangStemRange(lang: Lang, stemRange: LanguageStemRangeValue, excls: Option[List[LanguageExclusion]]): Boolean = {
    val cond1 = stemRange match {
      case LanguageStemRangeWildcard() => true
      case LanguageStemRangeLang(stem) => checkLanguageStem(lang, stem)
    }
    val cond2 = excls match {
      case None => true
      case Some(es) => es.forall(e => checkLanguageExclusion(lang,e))
    }
    cond1 && cond2
  }

  // Returns true if iri is not affected by the exclusion
  private def checkIRIExclusion(iri: IRI, exclusion: IRIExclusion): Boolean =
    exclusion match {
      case IRIRefExclusion(r) => iri.getLexicalForm != r.getLexicalForm
      case IRIStemExclusion(iriStem) => !iri.getLexicalForm.startsWith(iriStem.stem.getLexicalForm)
    }

  // Returns true if iri is not affected by the exclusion
  private def checkLiteralExclusion(lit: Literal, exclusion: LiteralExclusion): Boolean =
    exclusion match {
      case LiteralStringExclusion(r) => lit.getLexicalForm != r
      case LiteralStemExclusion(literalStem) => !lit.getLexicalForm.startsWith(literalStem.stem)
    }

  // Returns true if lang is not affected by the exclusion
  private def checkLanguageExclusion(lang: Lang, exclusion: LanguageExclusion): Boolean =
    exclusion match {
      case LanguageTagExclusion(stem) => lang.lang != stem.lang
      case LanguageStemExclusion(langStem) => !checkLanguageStem(lang,langStem.stem)
    }
}