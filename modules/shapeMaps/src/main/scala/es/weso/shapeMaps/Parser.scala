package es.weso.shapeMaps

import java.io.{ ByteArrayInputStream, InputStreamReader, Reader => JavaReader }
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.shapeMaps.parser.{ ShapeMapLexer, ShapeMapParser }
import org.antlr.v4.runtime._

object Parser extends LazyLogging {

  type Builder[A] = Either[String, A]

  def ok[A](x: A): Builder[A] = Right(x)

  def err[A](msg: String): Builder[A] = Left(msg)

  def removeBOM(str: String): String = {
    val UTF8_BOM = "\uFEFF"
    if (str.startsWith(UTF8_BOM)) {
      println(s"BOM detected and removed in $str")
      logger.info("BOM detected and removed")
      str.substring(1)
    } else {
      println(s"No BOM in $str")
      str
    }
  }

  def parse(
    str: String,
    base: Option[String],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, QueryShapeMap] = {
    val s = removeBOM(str)
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    println(s"Before parsing: $s with nodesPrefixMap=${nodesPrefixMap}, shapesPrefixMap=${shapesPrefixMap}")
    val r = parseSchemaReader(reader, base, nodesPrefixMap, shapesPrefixMap)
    println(s"Result of parsing $s = $r")
    r
  }

  def parseSchemaReader(
    reader: JavaReader,
    base: Option[String],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, QueryShapeMap] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: ShapeMapLexer = new ShapeMapLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShapeMapParser = new ShapeMapParser(tokens)

    val errorListener = new ParserErrorListener
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new ShapeMapsMaker(base: Option[String], nodesPrefixMap, shapesPrefixMap)
    val builder = maker.visit(parser.shapeMap()).asInstanceOf[Builder[QueryShapeMap]]
    val errors = errorListener.getErrors
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      builder
    }
  }

}
