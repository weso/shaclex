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
    value match {
      case IRIValue(iri) => node match {
        case i: IRI =>
          checkCond(iri == i, attempt,
            msgErr(s"${node.show} != ${i.show}"),
            s"${node.show} == ${i.show}")
        case _ => errStr(s"${node.show} != ${iri.show}")
      }
      case StringValue(s) => node match {
        case l: Literal => checkCond(s == l.getLexicalForm && l.dataType == xsd_string, attempt,
          msgErr(s"${node.show} != ${l}"),
          s"${node.show} == ${l}")
        case _ => errStr(s"${node.show} != ${value}")
      }
      case DatatypeString(s, iri) => {
        println(s"Check valueSet $value for $node")
        node match {
          case l: Literal =>
            checkCond(s == l.getLexicalForm && iri == l.dataType,
                      attempt,
                      msgErr(s"${node.show} != ${l}"),
                      s"${node.show} == ${l}")
          case _ => errStr(s"${node.show} != ${value}")
        }
      }
      case LangString(s, lang) => {
        node match {
          case LangLiteral(str, l) =>
            checkCond(s == str && lang == l, attempt,
              msgErr(s"${node.show} != ${value}"),
              s"${node.show} == ${value}")
          case _ => errStr(s"${node.show} != ${value}")
        }
      }
      case LanguageStem(stem) => {
        node match {
        case LangLiteral(x,lang) => {
          checkCond(lang.lang.startsWith(stem.lang),
                    attempt,
                    msgErr(s"${node.show} lang($lang) does not match ${stem}"),
                    s"${node.show} lang($lang) matches ${stem}")
        }
        case _ => errStr(s"${node.show} is not a language tagged literal")
      }
      }
      case ls@LanguageStemRange(stem,excls) => {
        node match {
          case LangLiteral(x,lang) => {
            checkCond(checkLangStemRange(lang, stem, excls),
              attempt,
              msgErr(s"${node.show} lang($lang) does not match ${ls}"),
              s"${node.show} lang($lang) matches ${ls}")
          }
          case _ => errStr(s"${node.show} is not a language tagged literal")
        }
      }
      case Language(langTag) => {
        node match {
          case LangLiteral(x,Lang(lang)) => checkCond(langTag.lang === lang, attempt,
            msgErr(s"${node.show} lang($lang) does not match ${langTag}"),
            s"${node.show} lang($lang) matches ${langTag}")
          case _ => errStr(s"${node.show} is not a language tagged literal")
        }
      }
      case IRIStem(stem) => node match {
        case i: IRI =>
          checkCond(i.getLexicalForm.startsWith(stem.getLexicalForm), attempt,
            msgErr(s"${node.show} does not match stem ${stem.show}"),
            s"${node.show} matches with stem ${stem.show}")
        case _ => errStr(s"${node.show} must be an IRI to match with IRI stem ${stem.show}")
      }
      case is@IRIStemRange(stem, excls) => {
        node match {
          case i: IRI => {
            checkCond(checkIRIStemRange(i, stem, excls),
              attempt,
              msgErr(s"${node.show} does not match IRI stem range $is"),
              s"${node.show} matches IRI stem range $is")
          }
          case _ => errStr(s"${node.show} is not an IRI")
        }
      }
      case LiteralStem(stem) => node match {
        case l: Literal =>
          checkCond(l.getLexicalForm.startsWith(stem), attempt,
            msgErr(s"${node.show} does not match stem ${stem.show}"),
            s"${node.show} matches with stem ${stem.show}")
        case _ => errStr(s"${node.show} must be a Literal to match with Literal stem ${stem.show}")
      }
      case ls@LiteralStemRange(stem, excls) => {
        node match {
          case l: Literal => {
            checkCond(checkLiteralStemRange(l, stem, excls),
              attempt,
              msgErr(s"${node.show} does not match Literal stem range $ls"),
              s"${node.show} matches Literal stem range $ls")
          }
          case _ => errStr(s"${node.show} is not a Literal")
        }
      }

/*      case _ => {
        logger.error(s"Not implemented checkValue: $value")
        errStr(s"Not implemented checkValue: $value")
      } */
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
      case LanguageStemRangeLang(stem) => lang.lang.startsWith(stem.lang)
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
      case LanguageStemExclusion(langStem) => !lang.lang.startsWith(langStem.stem.lang)
    }
}