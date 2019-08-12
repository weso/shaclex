package es.weso.rdf.sgraph

import cats.data._
import cats.implicits._
import es.weso.rdf.PREFIXES.`rdf:type`
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.{PrefixMap, RDFReader}

object RDF2SGraph {

  type Label = String
  type HRef = String
  type S[A] = State[SGraph, A]
  type Converter[A] = EitherT[S,String,A]

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

  def getGraph: Converter[SGraph] = EitherT.liftF[S,String,SGraph](StateT.get)
  def setGraph(g: SGraph): Converter[Unit] = EitherT.liftF[S,String,Unit](StateT.set(g))
  def ok[A](x:A): Converter[A] = EitherT.liftF(StateT.pure(x))
  def err[A](s: String): Converter[A] = EitherT.fromEither(s.asLeft[A])

  def rdf2sgraph(rdf: RDFReader): Either[String,SGraph] = {
    val pm = rdf.getPrefixMap()
    def cmb(u:Unit, t: RDFTriple): Converter[Unit] = for {
      edge <- rdfTriple2Edge(t, pm)
      g <- getGraph
      g1 = g.addEdge(edge)
      _ <- setGraph(g1)
    } yield ()

    for {
      ts <- rdf.rdfTriples
    } yield {
      ts.toList.foldM(())(cmb).value.run(SGraph.empty).value._1
    }
  }

}
