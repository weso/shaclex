package es.weso.shaclex.repl

// This code has been adapted from:
// https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/interactive/Completion.scala


/**
 * One of the results of a completion query.
 *
 * @param label         The label of this completion result, or the text that this completion result
 *                      should insert in the scope where the completion request happened.
 * @param description   The description of this completion result: the fully qualified name for
 *                      types, or the type for terms.
 * @param symbols       The symbols that are matched by this completion result.
 */
case class Completion(label: String, description: String, symbols: List[Symbol])

object Completion {

  def completions(): (Int, List[Completion]) = {
    (0,List())
  }

}