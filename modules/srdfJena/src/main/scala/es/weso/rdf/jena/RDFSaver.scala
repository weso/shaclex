package es.weso.rdf.jena

import cats._
import cats.data.{State, _}
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.PREFIXES.{rdf_first, rdf_nil, rdf_rest, rdf_type}
import es.weso.rdf.nodes._
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

  def optSaver[A](maybe: Option[A], saver: A => RDFSaver[RDFNode]): RDFSaver[Option[RDFNode]] = {
    maybe match {
      case None => ok(None)
      case Some(x) => saver(x).map(Some(_))
    }
  }

  def maybeAddTriple[A](node: RDFNode, pred: IRI, maybe: Option[RDFNode]): RDFSaver[Unit] = {
    maybe match {
      case None => State.pure(())
      case Some(x) => addTriple(node,pred, x)
    }
  }

  def addContent[A](x: A, node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    n <- saver(x)
    _ <- addTriple(node, pred, n)
  } yield ()

  def maybeAddContent[A](maybe: Option[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    maybeNode <- optSaver(maybe, saver)
    _ <- maybeAddTriple(node, pred, maybeNode)
  } yield ()

  def rdfBoolean(x: Boolean): RDFSaver[RDFNode] = x match {
    case true => ok(BooleanLiteral(true))
    case false => ok(BooleanLiteral(false))
  }

  def rdfInt(x: Int): RDFSaver[RDFNode] =
    ok(IntegerLiteral(x))

  def rdfString(x: String): RDFSaver[RDFNode] =
    ok(StringLiteral(x))


  def iri(i: IRI): RDFSaver[RDFNode] = ok(i)


  def addPrefix(alias: String, iri: IRI): RDFSaver[Unit] = {
    State.modify(_.addPrefix(alias,iri.str))
  }

  def listSaver[A](ls: List[A], saver: A => RDFSaver[RDFNode]): RDFSaver[List[RDFNode]] = {
    ls.map(saver(_)).sequence
  }

  def saveAsRDFList[A](ls: List[A], saver: A => RDFSaver[RDFNode]): RDFSaver[RDFNode] = for {
    nodes <- listSaver(ls,saver)
    ls <- mkRDFList(nodes)
  } yield ls

  def mkRDFList(ls: List[RDFNode]): RDFSaver[RDFNode] = ls match {
    case Nil => ok(rdf_nil)
    case x :: xs => for {
      node <- createBNode()
      _ <- addTriple(node,rdf_first,x)
      _ <- addContent(xs,node,rdf_rest,mkRDFList)
    } yield node
  }

  def addListContent[A](ls: List[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    listNode <- saveAsRDFList(ls,saver)
    _ <- addTriple(node, pred, listNode)
  } yield ()

  def maybeAddListContent[A](maybeLs: Option[List[A]], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] =
    maybeLs match {
      case None => ok(())
      case Some(ls) => addListContent(ls,node,pred,saver)
    }

  def addStarContent[A](ls: List[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    nodes <- listSaver(ls,saver)
    _ <- nodes.map(n => addTriple(node, pred, n)).sequence
  } yield ()

  def maybeAddStarContent[A](maybeLs: Option[List[A]], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] =
    maybeLs match {
      case None => ok(())
      case Some(ls) => addStarContent(ls,node,pred,saver)
    }


}

