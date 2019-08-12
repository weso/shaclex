package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._

class ValueCheckerTest extends ValueChecker(Schema.empty) with FunSpecLike with Matchers with EitherValues {

  describe("LanguageStem") {
    valueCheckerTest(LangLiteral("pepe", Lang("frc")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),Some(List())), false)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-cc")),
        LanguageStemRange(LanguageStemRangeLang(Lang("fr")),Some(List())), true)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-be")), LanguageStem(Lang("fr")), true)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-FR")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),
          Some(List(LanguageTagExclusion(Lang("fr-be")),
            LanguageTagExclusion(Lang("fr-cd")),
            LanguageTagExclusion(Lang("fr-ch")))))
      , true)
    valueCheckerTest(LangLiteral("septante", Lang("frc")),
      LanguageStemRange(LanguageStemRangeLang(Lang("")),
        Some(List(LanguageStemExclusion(LanguageStem(Lang("fr-be"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-cd"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-ch"))))))
      , true)
    valueCheckerTest(LangLiteral("septante", Lang("fr-bel")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),
        Some(List(LanguageStemExclusion(LanguageStem(Lang("fr-be"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-cd"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-ch"))))))
      , true)
  }

  def valueCheckerTest(node: RDFNode, value: ValueSetValue, ok: Boolean): Unit = {
    it(s"should checkValue($node, $value) and return $ok") {
      if (ok) valueChecker(node,value).fold(
        e => fail(s"Should check, but failed with message: $e"),
        msg => info(s"Passed with message: $msg")
      )
      else valueChecker(node,value).fold(
        e => info(s"Should fail and failed with message: $e"),
        msg => fail(s"Should fail but passed with message: $msg")
      )
    }
  }
}
