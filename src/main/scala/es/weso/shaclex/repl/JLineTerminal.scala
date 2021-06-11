package es.weso.shaclex

// import org.jline.reader
// import org.jline.reader.Parser.ParseContext
import org.jline.reader._
import org.jline.reader.impl.LineReaderImpl
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.TerminalBuilder
// import org.jline.utils.AttributedString

// This class has been adapted from: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/repl/JLineTerminal.scala
final class JLineTerminal extends java.io.Closeable {
  // import java.util.logging.{Logger, Level}
  // Logger.getLogger("org.jline").setLevel(Level.FINEST)

  private val terminal =
    TerminalBuilder.builder()
      .dumb(false) // fail early if not able to create a terminal
      .build()
  private val history = new DefaultHistory

  private def blue(str: String) = Console.BLUE + str + Console.RESET
  private def prompt()        = blue("shaclex> ")
//  private def newLinePrompt() = blue("     | ")

  /** Blockingly read line from `System.in`
   *
   *  This entry point into JLine handles everything to do with terminal
   *  emulation. This includes:
   *
   *  - Multi-line support
   *  - Copy-pasting
   *  - History
   *  - Syntax highlighting
   *  - Auto-completions
   *
   *  @throws EndOfFileException This exception is thrown when the user types Ctrl-D.
   */
  def readLine(
                completer: Completer // provide auto-completions
              ): String = {
    import LineReader.Option._
    import LineReader._
    val userHome = System.getProperty("user.home")
    val lineReader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .history(history)
      .completer(completer)
      .variable(HISTORY_FILE, s"$userHome/.shaclex_history") // Save history to file
      .variable(SECONDARY_PROMPT_PATTERN, "%M") // A short word explaining what is "missing",
      // this is supplied from the EOFError.getMissing() method
      .variable(LIST_MAX, 400)                  // Ask user when number of completions exceed this limit (default is 100).
      .variable(BLINK_MATCHING_PAREN, 0L)       // Don't blink the opening paren after typing a closing paren.
      .variable(WORDCHARS,
        LineReaderImpl.DEFAULT_WORDCHARS.filterNot("*?.[]~=/&;!#%^(){}<>".toSet)) // Finer grained word boundaries
      .option(INSERT_TAB, true)                 // At the beginning of the line, insert tab instead of completing.
      .option(AUTO_FRESH_LINE, true)            // if not at start of line before prompt, move to new line.
      .option(DISABLE_EVENT_EXPANSION, true)    // don't process escape sequences in input
      .build()

    lineReader.readLine(prompt)
  }

  def close(): Unit = terminal.close()


}