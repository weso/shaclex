package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.Path

case class Arc(path: Path, node: RDFNode) {

  override def toString: String = Arc.showArc.show(this)

}

object Arc {

  implicit lazy val showArc = new Show[Arc] {
    def show(arc: Arc): String = {
      s"(${arc.path.show},${arc.node.show})"
    }
  }

}