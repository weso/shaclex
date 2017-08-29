package es.weso.shapeMaps

import java.io.{ ByteArrayInputStream, InputStreamReader, Reader => JavaReader }
import java.nio.charset.StandardCharsets

import cats.data._
import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shapeMaps.parser.{ ShapeMapLexer, ShapeMapParser }
import es.weso.shapeMaps.parser.ShapeMapParser.{ StringContext => ShapeMapStringContext, _ }
import org.antlr.v4.runtime._

import scala.util.{ Failure, Success }

object Parser extends LazyLogging {

  type Builder[A] = Either[String, A]

  def ok[A](x: A): Builder[A] = Right(x)

  def err[A](msg: String): Builder[A] = Left(msg)

  def removeBOM(str: String): String = {
    val UTF8_BOM = "\uFEFF"
    if (str.startsWith(UTF8_BOM)) {
      logger.info("BOM detected and removed")
      str.substring(1)
    } else str
  }

  def parse(
    str: String,
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, InputShapeMap] = {
    val s = removeBOM(str)
    val reader: JavaReader = new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    parseSchemaReader(reader, nodesPrefixMap, shapesPrefixMap)
  }

  def parseSchemaReader(
    reader: JavaReader,
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, InputShapeMap] = {
    val input: ANTLRInputStream = new ANTLRInputStream(reader)
    val lexer: ShapeMapLexer = new ShapeMapLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShapeMapParser = new ShapeMapParser(tokens)

    val errorListener = new ParserErrorListener
    // lexer.removeErrorListeners()
    // parser.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new ShapeMapsMaker(nodesPrefixMap, shapesPrefixMap)
    val builder = maker.visit(parser.shapeMap()).asInstanceOf[Builder[InputShapeMap]]
    val errors = errorListener.getErrors
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      builder
    }
  }

}
