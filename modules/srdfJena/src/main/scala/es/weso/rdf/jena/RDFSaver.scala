package es.weso.rdf.jena

import cats._
import cats.data.{State, _}
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES.{rdf_first, rdf_nil, rdf_rest, rdf_type}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.triples.RDFTriple

trait RDFSaver {
  type RDFSaver[A] = State[RDFAsJenaModel,A]

  def ok[A](x:A): RDFSaver[A] = StateT.pure(x)

  def saveList[A](ls: List[A], f: A => RDFSaver[Unit]): RDFSaver[Unit] = {
    ls.map(f(_)).sequence.map(_ => ())
  }

  def saveToRDFList[A](ls: List[A], f: A => RDFSaver[RDFNode]): RDFSaver[RDFNode] = ls match {
    case Nil => State.pure(rdf_nil)
    case x :: xs => for {
      nodeX <- f(x)
      bNode <- createBNode
      _ <- addTriple(bNode,rdf_first,nodeX)
      rest <- saveToRDFList(xs,f)
      _ <- addTriple(bNode,rdf_rest,rest)
    } yield bNode
  }

  def addTriple(s: RDFNode, p: IRI, o: RDFNode): RDFSaver[Unit] =
    State.modify(_.addTriple(RDFTriple(s,p,o)))

  def addPrefixMap(pm: PrefixMap): RDFSaver[Unit] =
    State.modify(_.addPrefixMap(pm))

  def addPrefix(alias: String, value: String): RDFSaver[Unit] =
    State.modify(_.addPrefix(alias,value))

  def createBNode(): RDFSaver[RDFNode] = for {
    rdf <- State.get[RDFAsJenaModel]
    (bNode,newRdf) = rdf.createBNode
    _ <- State.set[RDFAsJenaModel](newRdf)
  } yield bNode

  def makeId(v: Option[IRI]): RDFSaver[RDFNode] = v match {
    case None => for {
      bNode <- createBNode()
    } yield(bNode)
    case Some(iri) =>
      State.pure(iri)
  }

}

