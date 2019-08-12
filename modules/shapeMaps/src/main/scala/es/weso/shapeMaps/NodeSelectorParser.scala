package es.weso.shapeMaps

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.shapeMaps.parser.{NodeSelectorLexer, NodeSelectorParser}
import org.antlr.v4.runtime._

object ParserNodeSelector extends LazyLogging {

  type Builder[A] = Either[String, A]

  def ok[A](x: A): Builder[A] = Right(x)

  def err[A](msg: String): Builder[A] = Left(msg)

  def removeBOM(str: String): String = {
    val UTF8_BOM = "\uFEFF"
    if (str.startsWith(UTF8_BOM)) {
      logger.info("BOM detected and removed")
      str.substring(1)
    } else {
      str
    }
  }

  def parse(
    str: String,
    base: Option[String],
    nodesPrefixMap: PrefixMap): Either[String, NodeSelector] = {
    val s = removeBOM(str)
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    val r = parseSchemaReader(reader, base, nodesPrefixMap)
    println(s"NodeSelector:parse: $r, base: $base")
    r
  }

  def parseSchemaReader(
    reader: JavaReader,
    base: Option[String],
    nodesPrefixMap: PrefixMap
    ): Either[String, NodeSelector] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: NodeSelectorLexer = new NodeSelectorLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: NodeSelectorParser = new NodeSelectorParser(tokens)

    val errorListener = new ParserErrorListener
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new NodeSelectorMaker(
      base: Option[String],
      nodesPrefixMap)
    val builder = maker.visit(parser.nodeSelector()).asInstanceOf[Builder[NodeSelector]]
    val errors = errorListener.getErrors
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      builder
    }
  }

}
