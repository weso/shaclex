package es.weso.shex.compact

import java.io.{ ByteArrayInputStream, InputStreamReader, Reader => JavaReader }

import cats.data._
import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.parser._
import org.antlr.v4.runtime._
import java.nio.charset.StandardCharsets

import es.weso.utils.FileUtils

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
    val v: Builder[A] = EitherT.left[A](r)
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

  def updateStart(s: Start): Builder[Unit] = {
    // logger.info(s"New start: $s")
    updateState(_.copy(start = s))
  }

  def addShape(label: ShapeLabel, expr: ShapeExpr): Builder[Unit] = {
    updateState(s => s.copy(shapesMap = s.shapesMap + (label -> expr)))
  }

  def addPrefix(prefix: Prefix, iri: IRI): Builder[Unit] = {
    updateState(s => {
      val newS = s.copy(prefixMap = s.prefixMap.addPrefix(prefix, iri))
      // logger.info(s"Updating prefix map. New prefix map=${newS.prefixMap}")
      newS
    })
  }
  def parseSchema(str: String, base: Option[String]): Either[String, Schema] = {
    val UTF8_BOM = "\uFEFF"
    val s =
      if (str.startsWith(UTF8_BOM)) {
        // logger.info("BOM detected and removed")
        str.substring(1)
      } else str
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    // logger.info(s"str:\n$s")
    parseSchemaReader(reader, base)
  }

  def parseSchemaFromFile(fileName: String, base: Option[String]): Either[String, Schema] = for {
    reader <- FileUtils.getStream(fileName)
    schema <- parseSchemaReader(reader, base)
  } yield schema

  def parseSchemaReader(reader: JavaReader, base: Option[String]): Either[String, Schema] = {
    val input: ANTLRInputStream = new ANTLRInputStream(reader)
    val lexer: ShExDocLexer = new ShExDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShExDocParser = new ShExDocParser(tokens)

    val errorListener = new ParserErrorListener
    // lexer.removeErrorListeners()
    // parser.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new SchemaMaker() // new DebugSchemaMaker()
    val builder = maker.visit(parser.shExDoc()).asInstanceOf[Builder[Schema]]
    val errors = errorListener.getErrors
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      run(builder, base)._2
    }
  }

  def run[A](c: Builder[A],
             base: Option[String]
            ): (BuilderState, Either[String, A]) = c.value.run(initialState(base)).value

  def initialState(base: Option[String]) =
    BuilderState(
      PrefixMap.empty,
      base.map(IRI(_)),
      None,
      Map())

  case class BuilderState(
    prefixMap: PrefixMap,
    base: Option[IRI],
    start: Option[ShapeExpr],
    shapesMap: ShapesMap)

}
