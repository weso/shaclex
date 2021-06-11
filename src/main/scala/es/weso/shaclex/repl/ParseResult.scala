package es.weso.shaclex.repl

/** A parsing result from string input */
sealed trait ParseResult

/** Parsed result is simply a newline */
case object Newline extends ParseResult

/** `ctrl-c` obtained from input string */
case object SigKill extends ParseResult

/** A command is on the format:
 *
 *  ```none
 *  :commandName <optional arguments...>
 *  ```
 *  The `Command` trait denotes these commands
 */
sealed trait Command extends ParseResult

/** An unknown command that will not be handled by the REPL */
case class UnknownCommand(cmd: String) extends Command

/** An ambiguous prefix that matches multiple commands */
case class AmbiguousCommand(cmd: String, matchingCommands: List[String]) extends Command

/** `:load <path>` interprets a scala file as if entered line-by-line into
 *  the REPL
 */
case class Load(path: String) extends Command
object Load {
  val command: String = ":load"
}

/** `:quit` exits the repl */
case object Quit extends Command {
  val command: String = ":quit"
}

/** `:help` shows the different commands implemented by the Dotty repl */
case object Help extends Command {
  val command: String = ":help"
  val text: String =
    """The REPL has several commands available:
      |
      |:help                    print this summary
      |:load <path>             interpret lines in a file
      |:quit                    exit the interpreter
    """.stripMargin
}

object ParseResult {

  private val CommandExtract = """(:[\S]+)\s*(.*)""".r

  private val commands: List[(String, String => ParseResult)] = List(
    Quit.command -> (_ => Quit),
    Help.command -> (_  => Help),
    Load.command -> (arg => Load(arg)),
  )

  def apply(source: SourceFile)(implicit state: State): ParseResult = {
    val sourceCode = source.content().mkString
    sourceCode match {
      case "" => Newline
      case CommandExtract(cmd, arg) => {
        val matchingCommands = commands.filter{ case (command, _) => command.startsWith(cmd) }
        matchingCommands match {
          case Nil => UnknownCommand(cmd)
          case (_, f) :: Nil => f(arg)
          case multiple => AmbiguousCommand(cmd, multiple.map(_._1))
        }
      }
      case _ => UnknownCommand(sourceCode)
    }
  }

  def apply(sourceCode: String)(implicit state: State): ParseResult =
    apply(SourceFile.virtual("REPL", sourceCode))
}