package es.weso.shex.compact

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}

import cats.data._
import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.parser._
import org.antlr.v4.runtime._
import java.nio.charset.StandardCharsets

import es.weso.utils.FileUtils

import scala.collection.immutable.ListMap

object Parser extends LazyLogging {

  type S[A] = State[BuilderState, A]
  type Builder[A] = EitherT[S, String, A]

  // type PrefixMap = Map[Prefix,IRI]
  type Start = Option[ShapeExpr]
  type ShapesMap = ListMap[ShapeLabel, ShapeExpr]
  type TripleExprMap = Map[ShapeLabel, TripleExpr]

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

  def getTripleExprMap: Builder[TripleExprMap] =
    getState.map(_.tripleExprMap)

  def getState: Builder[BuilderState] =
    EitherT.liftF[S, String, BuilderState](StateT.inspect(identity))

  def getBase: Builder[Option[IRI]] =
    getState.map(_.base)

  def getStart: Builder[Start] =
    getState.map(_.start)

  def addBase(base: IRI): Builder[Unit] =
    updateState(_.copy(base = Some(base)))

  def updateState(fn: BuilderState => BuilderState): Builder[Unit] = {
    EitherT.liftF[S, String, Unit](StateT.modify(fn))
  }

  def updateStart(s: Start): Builder[Unit] = {
    // logger.info(s"New start: $s")
    updateState(_.copy(start = s))
  }

  // TODO: Check what to do if the label is already assigned
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

  def addTripleExprLabel(label: ShapeLabel, te: TripleExpr): Builder[TripleExpr] = for {
    s <- getState
    _ <- s.tripleExprMap.get(label) match {
      case None => ok(())
      case Some(otherTe) => err(s"Label $label has been assigned to ${otherTe} and can't be assigned to $te")
    }
    _ <- updateState(s => s.copy(tripleExprMap = s.tripleExprMap + (label -> te)))
  } yield (te.addId(label))

  def parseSchema(str: String, base: Option[String]): Either[String, Schema] = {
    val UTF8_BOM = "\uFEFF"
    val s =
      if (str.startsWith(UTF8_BOM)) {
        logger.info("BOM detected and removed")
        str.substring(1)
      } else str
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    logger.debug(s"s:$s")
    parseSchemaReader(reader, base)
  }

  def parseSchemaFromFile(fileName: String, base: Option[String]): Either[String, Schema] = for {
    reader <- FileUtils.getStream(fileName)
    schema <- parseSchemaReader(reader, base)
  } yield schema

  def parseSchemaReader(reader: JavaReader, base: Option[String]): Either[String, Schema] = {
    val input: CharStream = CharStreams.fromReader(reader)
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
      ListMap(),
      Map())

  case class BuilderState(prefixMap: PrefixMap,
                          base: Option[IRI],
                          start: Option[ShapeExpr],
                          shapesMap: ShapesMap,
                          tripleExprMap: TripleExprMap
                         )

}
