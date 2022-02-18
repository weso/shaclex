package es.weso.shaclex.repl

import com.typesafe.scalalogging.LazyLogging
import es.weso.shaclex.{JLineTerminal, MainOpts}
import org.jline.reader._

import java.io.PrintStream
import scala.annotation.tailrec
// import scala.collection.JavaConverters._

case class State(index: Int)

class Repl(opts: MainOpts, out: PrintStream = Console.out) extends LazyLogging {

  final def initialState: State = State(0)

  final def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal

    def readLine(state: State): ParseResult = {

      // TODO: Add better completions
      val completer: Completer = { (_, _, candidates) =>
        candidates
      }

      try {
        val line = terminal.readLine(completer)
        ParseResult(line)(state)
      } catch {
        case _: EndOfFileException | _: UserInterruptException => // Ctrl+D or Ctrl+C
          Quit
      }
    }

    @tailrec def loop(state: State): State = {
      val res = readLine(state)
      if (res == Quit) state
      else loop(interpret(res)(state))
    }

    try withRedirectedOutput { loop(initialState) } finally terminal.close()
  }

  // redirecting the output allows us to test `println` in scripted tests
  private def withRedirectedOutput(op: => State): State = {
    val savedOut = System.out
    val savedErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      op
    } finally {
      System.setOut(savedOut)
      System.setErr(savedErr)
    }
  }

  private def interpret(res: ParseResult)(implicit state: State): State = {
    val newState = res match {
      case cmd: Command =>
        interpretCommand(cmd)

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
    newState
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command)(implicit state: State): State = cmd match {
    case UnknownCommand(cmd) =>
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state

    case AmbiguousCommand(cmd, matching) =>
      out.println(s""""$cmd" matches ${matching
        .mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      state

    case Help =>
      out.println(Help.text)
      state

    case Load(path) =>
      out.println(s"Load: $path")
      state

    case Quit =>
      // end of the world!
      state
  }

}
