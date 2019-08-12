package es.weso.rdf.operations
import es.weso.rdf.nodes._
import es.weso.rdf._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple

object Graph {

  /**
  * traverse the outgoing nodes of a node
    * It currently only takes into account the ourgoing arcs
    * @param node starting node
    * @param rdf RDFReader
    * @return list of visited nodes
    */
  def traverse(node: RDFNode, rdf: RDFReader): Either[String,List[RDFNode]] = {
    // println(s"Traversing from $node\nRDF:\n${rdf.serialize("TURTLE").getOrElse("")}")

    def outgoing(node: RDFNode): Either[String, List[RDFNode]] = for {
     ts <- rdf.triplesWithSubject(node)
    } yield ts.toList.map(_.obj)

    def outgoingNotVisited(node: RDFNode,
                           visited: List[RDFNode]
                          ): Either[String, List[RDFNode]] = for {
      os <- outgoing(node)
    } yield os.filter(x => x != node && !visited.contains(x))

    // @annotation.tailrec
    // TODO: Refactor to be tailrec again
    def loop(stack: List[RDFNode],
             visited: List[RDFNode]
            ): Either[String,List[RDFNode]] = stack match {
      case Nil => Right(visited)
      case head :: tail => for {
        onv <- outgoingNotVisited(head, visited)
        rs <- loop(onv ++ tail, head :: visited)
      } yield rs
    }
    loop(List(node),List())
  }

  // The following declarations avoid IntelliJ wrong positive
  type ES[A] = Either[String,A]
  def sequenceU[A](xs: List[Either[String,A]]): Either[String,List[A]] = xs.sequence[ES,A]

  def traverseWithArcs(node: RDFNode, rdf: RDFReader): Either[String, (List[RDFNode], List[RDFTriple])] = for {
    nodes <- traverse(node,rdf)
    triples <- sequenceU(nodes.map(rdf.triplesWithSubject(_))).map(_.flatten)
  } yield (nodes, triples)

}