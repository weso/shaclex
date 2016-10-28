package es.weso.shex.compact

import java.util

import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging._
import es.weso.rdf.PREFIXES._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.parser.ShExDocParser.{StringContext => ShExStringContext, _}
import es.weso.shex.parser._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn.ATNConfigSet
import org.antlr.v4.runtime.dfa.DFA
import scala.collection.JavaConverters._

object Parser extends LazyLogging {

  type S[A] = State[BuilderState, A]
  type Builder[A] = EitherT[S, String, A]

  // type PrefixMap = Map[Prefix,IRI]
  type Start = Option[ShapeExpr]
  type ShapesMap = Map[ShapeLabel, ShapeExpr]

  def ok[A](x: A): Builder[A] =
    EitherT.pure(x)

  def err[A](msg: String): Builder[A] = {
    val r: S[String] = StateT.pure(msg)
    val v: Builder[A] = EitherT.left[S, String, A](r)
    v
  }

  def getPrefixMap: Builder[PrefixMap] =
    getState.map(_.prefixMap)

  def getShapesMap: Builder[ShapesMap] =
    getState.map(_.shapesMap)

  def getState: Builder[BuilderState] =
    EitherT.liftT[S, String, BuilderState](StateT.inspect(identity))

  def getBase: Builder[Option[IRI]] =
    getState.map(_.base)

  def getStart: Builder[Start] =
    getState.map(_.start)

  def addBase(base: IRI): Builder[Unit] =
    updateState(_.copy(base = Some(base)))

  def updateState(fn: BuilderState => BuilderState): Builder[Unit] = {
    EitherT.liftT[S, String, Unit](StateT.modify(fn))
  }

  def updateStart(s: Start): Builder[Unit] =
    updateState(_.copy(start = s))

  def addShape(label: ShapeLabel, expr: ShapeExpr): Builder[Unit] =
    updateState(s => s.copy(shapesMap = s.shapesMap + (label -> expr)))

  def addPrefix(prefix: Prefix, iri: IRI): Builder[Unit] =
    updateState(s => s.copy(prefixMap = s.prefixMap.addPrefix(prefix, iri)))

  def parseSchema(str: String): Either[String, Schema] = {
    val input: ANTLRInputStream = new ANTLRInputStream(str)
    val lexer: ShExDocLexer = new ShExDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShExDocParser = new ShExDocParser(tokens)

    var errors = new scala.collection.mutable.Queue[String]

    val errorListener = new ANTLRErrorListener {
      override def reportContextSensitivity(recognizer: Parser,
                                            dfa: DFA,
                                            startIndex: Int,
                                            stopIndex: Int,
                                            prediction: Int,
                                            configs: ATNConfigSet): Unit = {

      }

      override def reportAmbiguity(recognizer: Parser,
                                   dfa: DFA,
                                   startIndex: Int,
                                   stopIndex: Int,
                                   exact: Boolean,
                                   ambigAlts: util.BitSet,
                                   configs: ATNConfigSet): Unit = {}

      override def reportAttemptingFullContext(recognizer: Parser,
                                               dfa: DFA,
                                               startIndex: Int,
                                               stopIndex: Int,
                                               conflictingAlts: util.BitSet,
                                               configs: ATNConfigSet): Unit = {}

      override def syntaxError(recognizer: Recognizer[_, _],
                               offendingSymbol: scala.Any,
                               line: Int,
                               charPositionInLine: Int,
                               msg: String, e: RecognitionException): Unit = {
        val str = s"Error ${errors.length} line $line:$charPositionInLine $msg"
        errors += str
      }
    }

    lexer.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)

    val maker = new SchemaMaker()
    val builder = maker.visit(parser.shExDoc()).asInstanceOf[Builder[Schema]]
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      run(builder)._2
    }
  }

  def run[A](c: Builder[A]): (BuilderState, Either[String, A]) = c.value.run(initialState).value

  def initialState =
    BuilderState(
      PrefixMap.empty,
      None,
      None,
      Map()
    )

  case class BuilderState(prefixMap: PrefixMap,
                          base: Option[IRI],
                          start: Option[ShapeExpr],
                          shapesMap: ShapesMap
                         )

}
