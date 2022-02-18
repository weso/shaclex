package es.weso.shaclex.repl

class SourceFile(computeContent: => String) {
  private var myContent: String = null

  def content(): String = {
    if (myContent == null) myContent = computeContent
    myContent
  }
}

object SourceFile {

  def virtual(name: String, content: String, maybeIncomplete: Boolean = false): SourceFile = {
    val src = new SourceFile(content)
    src
  }

}