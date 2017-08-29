package es.weso.shex.compact
import java.util

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex._
import es.weso.shex.parser.ShExDocBaseVisitor
import es.weso.shex.compact.Parser._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.ParseTree
// import org.antlr.v4.runtime.atn.ATNConfigSet
// import org.antlr.v4.runtime.dfa.DFA
import es.weso.shex.parser.ShExDocParser.{ StringContext => ShExStringContext, _ }
import es.weso.shex.parser._
import scala.collection.JavaConverters._

/**
 * Visits the AST and builds the corresponding ShEx abstract syntax
 */
class DebugSchemaMaker extends ShExDocBaseVisitor[Any] with LazyLogging {

  override def visitShExDoc(ctx: ShExDocContext): Builder[Schema] = {
    println("Visiting ShExDoc")
    visitChildren(ctx)
    ok(Schema.empty)
  }

}
