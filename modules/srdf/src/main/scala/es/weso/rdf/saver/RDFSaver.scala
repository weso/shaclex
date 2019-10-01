package es.weso.rdf.saver

import cats.data.{State, _}
import cats.implicits._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.{PrefixMap, RDFBuilder}

trait RDFSaver {
  type RDFSaver[A] = State[RDFBuilder, A]

  def ok[A](x: A): RDFSaver[A] = StateT.pure(x)

  def saveList[A](ls: List[A], f: A => RDFSaver[Unit]): RDFSaver[Unit] = {
    sequence(ls.map(f(_))).map(_ => ())
  }

  def listSaver[A](ls: List[A], saver: A => RDFSaver[RDFNode]): RDFSaver[List[RDFNode]] = {
    sequence(ls.map(saver(_)))
  }

  def saveToRDFList[A](ls: List[A], f: A => RDFSaver[RDFNode]): RDFSaver[RDFNode] = ls match {
    case Nil => State.pure(`rdf:nil`)
    case x :: xs => for {
      nodeX <- f(x)
      bNode <- createBNode()
      _ <- addTriple(bNode, `rdf:first`, nodeX)
      rest <- saveToRDFList(xs, f)
      _ <- addTriple(bNode, `rdf:rest`, rest)
    } yield bNode
  }

  def addTriple(s: RDFNode, p: IRI, o: RDFNode): RDFSaver[Unit] =
    State.modify(_.addTriple(RDFTriple(s, p, o)).right.get)

  def addTripleObjects(s:RDFNode, p: IRI, os: List[RDFNode]): RDFSaver[Unit] = {
    saveList(os, (o: RDFNode) => addTriple(s,p,o))
  }

  def createBNode(): RDFSaver[RDFNode] = for {
    rdf <- State.get[RDFBuilder]
    (bNode, newRdf) = rdf.createBNode
    _ <- State.set[RDFBuilder](newRdf)
  } yield bNode

  def makePath(path: SHACLPath): RDFSaver[RDFNode] = path match {
    case PredicatePath(iri) => State.pure(iri)
    case InversePath(p) => for {
      node <- createBNode()
      pathNode <- makePath(p)
      _ <- addTriple(node, `sh:inversePath`, pathNode)
    } yield node
    case ZeroOrOnePath(p) => for {
      node <- createBNode()
      pathNode <- makePath(p)
      _ <- addTriple(node, `sh:zeroOrOnePath`, pathNode)
    } yield node
    case ZeroOrMorePath(p) => for {
      node <- createBNode()
      pathNode <- makePath(p)
      _ <- addTriple(node, `sh:zeroOrMorePath`, pathNode)
    } yield node
    case OneOrMorePath(p) => for {
      node <- createBNode()
      pathNode <- makePath(p)
      _ <- addTriple(node, `sh:oneOrMorePath`, pathNode)
    } yield node
    case SequencePath(ps) => for {
      list <- saveToRDFList(ps.toList,makePath)
    } yield list
    case AlternativePath(ps) => for {
      node <- createBNode()
      list <- saveToRDFList(ps.toList,makePath)
      _ <- addTriple(node,`sh:alternativePath`,list)
    } yield node
    case _ => throw new Exception(s"Unimplemented conversion of path: $path")
  }


  def makeId(v: Option[IRI]): RDFSaver[RDFNode] = v match {
    case None => createBNode()
    case Some(iri) => State.pure(iri)
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
      case Some(x) => addTriple(node, pred, x)
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

  def rdfBoolean(x: Boolean): RDFSaver[RDFNode] =
    ok(BooleanLiteral(x))

  def rdfInt(x: Int): RDFSaver[RDFNode] =
    ok(IntegerLiteral(x))

  def rdfString(x: String): RDFSaver[RDFNode] =
    ok(StringLiteral(x))

  def iri(i: IRI): RDFSaver[RDFNode] = ok(i)

  def addPrefix(alias: String, iri: IRI): RDFSaver[Unit] = {
    State.modify(_.addPrefix(alias, iri))
  }

  def addPrefixMap(pm: PrefixMap): RDFSaver[Unit] =
    State.modify(_.addPrefixMap(pm))


  def saveAsRDFList[A](ls: List[A], saver: A => RDFSaver[RDFNode]): RDFSaver[RDFNode] = for {
    nodes <- listSaver(ls, saver)
    ls <- mkRDFList(nodes)
  } yield ls

  def mkRDFList(ls: List[RDFNode]): RDFSaver[RDFNode] = ls match {
    case Nil => ok(`rdf:nil`)
    case x :: xs => for {
      node <- createBNode()
      _ <- addTriple(node, `rdf:first`, x)
      _ <- addContent(xs, node, `rdf:rest`, mkRDFList)
    } yield node
  }

  def addListContent[A](ls: List[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    listNode <- saveAsRDFList(ls, saver)
    _ <- addTriple(node, pred, listNode)
  } yield ()

  def maybeAddListContent[A](maybeLs: Option[List[A]], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] =
    maybeLs match {
      case None => ok(())
      case Some(ls) => addListContent(ls, node, pred, saver)
    }

  def addStarContent[A](ls: List[A], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] = for {
    nodes <- listSaver(ls, saver)
    _ <- nodes.map(n => addTriple(node, pred, n)).sequence
  } yield ()

  def maybeAddStarContent[A](maybeLs: Option[List[A]], node: RDFNode, pred: IRI, saver: A => RDFSaver[RDFNode]): RDFSaver[Unit] =
    maybeLs match {
      case None => ok(())
      case Some(ls) => addStarContent(ls, node, pred, saver)
    }

  def sequence[A](ls: List[RDFSaver[A]]): RDFSaver[List[A]] =
    ls.sequence[RDFSaver,A]
}

