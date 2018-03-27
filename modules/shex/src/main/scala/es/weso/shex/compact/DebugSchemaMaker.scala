package es.weso.shex.compact
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex._
import es.weso.shex.parser.ShExDocBaseVisitor
import es.weso.shex.compact.Parser._
import es.weso.shex.parser.ShExDocParser.ShExDocContext

/**
 * Visits the AST and builds the corresponding ShEx abstract syntax
 */
class DebugSchemaMaker extends ShExDocBaseVisitor[Any] with LazyLogging {

  override def visitShExDoc(ctx: ShExDocContext): Builder[Schema] = {
    logger.debug("Visiting ShExDoc")
    visitChildren(ctx)
    ok(Schema.empty)
  }

}
