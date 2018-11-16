package es.weso.rdf.dot

import cats.implicits._
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.PREFIXES._
import cats.data.{State, _}


object RDF2Dot {

  type Label = String
  type HRef = String
  type Converter[A] = State[DotGraph, A]

  def rdfTriple2Edge(t: RDFTriple, pm: PrefixMap): Converter[Edge] = for {
    n1 <- rdfNode2Node(t.subj, pm)
    n2 <- rdfNode2Node(t.obj, pm)
    e <- predicate2href(t.pred, pm)
  } yield Edge(n1,n2,e._1, e._2)

  def predicate2href(pred: IRI, pm: PrefixMap): Converter[(Label, HRef)] = pred match {
    case `rdf:type` => ok(("a", pred.str))
    case _ => ok((pm.qualify(pred), pred.str))
  }

  def rdfNode2Node(node: RDFNode, pm: PrefixMap): Converter[Node] = for {
    g <- getGraph
    (g1,n) = g.addNode(node,pm)
    _ <- setGraph(g1)
  } yield n

  def getGraph: Converter[DotGraph] = StateT.get
  def setGraph(g: DotGraph): Converter[Unit] = StateT.set(g)
  def ok[A](x:A): Converter[A] = StateT.pure(x)

  def rdf2dot(rdf: RDFReader): DotGraph = {
    val pm = rdf.getPrefixMap()
    def cmb(u:Unit, t: RDFTriple): Converter[Unit] = for {
      edge <- rdfTriple2Edge(t, pm)
      g <- getGraph
      g1 = g.addEdge(edge)
      _ <- setGraph(g1)
    } yield ()
    rdf.rdfTriples.toList.foldM(())(cmb).run(DotGraph.empty).value._1
  }

}