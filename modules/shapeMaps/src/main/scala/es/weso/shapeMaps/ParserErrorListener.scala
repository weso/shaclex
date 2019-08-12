package es.weso.shapeMaps

import java.util

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA

class ParserErrorListener extends ANTLRErrorListener {

  private val errors = new scala.collection.mutable.Queue[String]

  def getErrors(): List[String] = errors.toList

  override def reportContextSensitivity(
    recognizer: Parser,
    dfa: DFA,
    startIndex: Int,
    stopIndex: Int,
    prediction: Int,
    configs: ATNConfigSet): Unit = {

  }

  override def reportAmbiguity(
    recognizer: Parser,
    dfa: DFA,
    startIndex: Int,
    stopIndex: Int,
    exact: Boolean,
    ambigAlts: util.BitSet,
    configs: ATNConfigSet): Unit = {}

  override def reportAttemptingFullContext(
    recognizer: Parser,
    dfa: DFA,
    startIndex: Int,
    stopIndex: Int,
    conflictingAlts: util.BitSet,
    configs: ATNConfigSet): Unit = {}

  override def syntaxError(
    recognizer: Recognizer[_, _],
    offendingSymbol: scala.Any,
    line: Int,
    charPositionInLine: Int,
    msg: String,
    e: RecognitionException): Unit = {
    val str = s"Error at $line:$charPositionInLine $msg\n"
    // if (e.getCtx() != null) println(s"Rule index: ${e.getCtx.getRuleIndex}\nContext text: ${e.getCtx.getText}")
    errors += str
  }
}
