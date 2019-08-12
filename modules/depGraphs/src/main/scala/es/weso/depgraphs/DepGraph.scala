package es.weso.depgraphs

trait DepGraph[Node] {

  def nodes: Set[Node]

  def addNode(node: Node): DepGraph[Node]

  def addEdge(node1: Node, posNeg: PosNeg, node2: Node): DepGraph[Node]

  def addPosEdge(node1: Node, node2: Node): DepGraph[Node] =
    addEdge(node1, Pos, node2)

  def addNegEdge(node1: Node, node2: Node): DepGraph[Node] =
    addEdge(node1, Neg, node2)

  def outEdges(node: Node): Either[String, Set[(PosNeg, Node)]]

  def empty: DepGraph[Node]

  def containsNegCycle: Boolean = {
    !negCycles.isEmpty
  }

  def negCycles: Set[Set[(Node,Node)]]

  def oddNegCycles: Set[Set[(Node,Node)]] = {
    val nc = negCycles
  //  println(s"Neg cycles: $nc")
    nc.filter(countNegLinks(_) % 2 == 1)
  }

  def countNegLinks(nodes: Set[(Node,Node)]
                   ): Int = nodes.size match {
    case 0 => 0
    case _ => {
      val negs = nodes.map{
        case (n1,n2) => (n1,n2,haveNegativeLink(n1, n2))
      }.filter(_._3 == true)
      val n = negs.size
//      println(s"#NegLinks of $nodes=$n\nnegs=${negs}")
      n
    }
  }

  def edgeBetween(node1: Node, node2: Node): Option[PosNeg]

  private def haveNegativeLink(node1: Node, node2: Node): Boolean = {
    edgeBetween(node1,node2) match {
      case Some(Neg) => true
      case Some(Pos) => false
      case Some(Both) => true
      case None => false
    }
  }

  def showEdges(showNode: Node => String = (x => x.toString)): String

  /**
   * Checks if this graph is isomorphic with another one
   * @param other the other dependency graph
   * @return Left(msg) if it is not isomorphic or Right(()) if it is isomorphic
   */
  def isomorphicWith(other: DepGraph[Node]): Either[String, Unit]
}

object DepGraph {
  def empty[Node] = DepGraphJGraphT[Node]().empty

  def makeGraph[Node](deps: Set[(Node, Set[(PosNeg, Node)])]): DepGraph[Node] = {
    def combine(pair: (Node, Set[(PosNeg, Node)]), g: DepGraph[Node]): DepGraph[Node] = {
      val (source, edges) = pair
      edges.foldRight(g)((pair, current) => {
        val (posNeg, target) = pair
        current.addEdge(source, posNeg, target)
      })
    }
    deps.foldRight(empty[Node])(combine)
  }
}

