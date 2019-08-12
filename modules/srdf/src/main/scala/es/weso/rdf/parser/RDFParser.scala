package es.weso.rdf.parser

import es.weso.rdf.nodes._
import scala.util._
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.triples.RDFTriple
import cats._
import cats.implicits._

/**
 * Exceptions raised by the RDFParser
 * case class RDFParserException(msg: String)
 * extends Exception(s"RDFParserException: " + msg)
 */

/**
 * Obtains data from an RDFReader
 *
 * <p>
 * The approach is similar to parser combinators but instead of sequence of characters,
 * we have RDF graphs available through an RDFReader
 *
 */
trait RDFParser {

  /**
   * An RDFParser of values of type `a` takes a pointed node `RDFNode`
   * and an `RDFReader` and tries to obtain a value of type `a`
   */
  // TODO: refactor this code to use a reader monad...
  type ES[A] = Either[String,A]
  type RDFParser[A] = (RDFNode, RDFReader) => ES[A]

  implicit val applicativeRDFParser = new Applicative[RDFParser] {
    def pure[A](x: A) = (n, rdf) => Right(x)

    def ap[A, B](ff: RDFParser[A => B])(fa: RDFParser[A]): RDFParser[B] = (n, f) => {
      fa(n, f) match {
        case Right(a) => ff(n, f) match {
          case Right(f) => Right(f(a))
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
    }
  }

  /*
   * returns an RDFParser which returns the IRI associated with a predicate
   * @param p predicate
   */
  def iriFromPredicate(p: IRI): RDFParser[IRI] = { (n, rdf) =>
    for {
      node <- objectFromPredicate(p)(n, rdf)
      iri <- node match {
        case i: IRI => parseOk(i)
        case _ => parseFail("Value of predicate " + p + " must be a IRI but it is: " + node)
      }
    } yield iri
  }

  /**
   * Returns the String associated with a predicate `p`
   * @param p predicate
   * @return An RDFParser that returns the String associated with that predicate
   *
   */
  def stringFromPredicate(p: IRI): RDFParser[String] = { (n, rdf) =>
    for {
      obj <- objectFromPredicate(p)(n, rdf)
      str <- obj match {
        case StringLiteral(str) => parseOk(str)
        case _ => parseFail("Value of predicate " + p + " must be a string literal but it is: " + obj)
      }
    } yield str
  }

  /**
    * Returns the Decimal literal associated with a predicate `p`
    * @param p predicate
    * @return An RDFParser that returns the decimal literal associated with that predicate
    *
    */
  def decimalLiteralFromPredicate(p: IRI): RDFParser[DecimalLiteral] = { (n, rdf) =>
    for {
      obj <- objectFromPredicate(p)(n, rdf)
      node <- obj match {
        case d: DecimalLiteral => parseOk(d)
        case _ => parseFail("Value of predicate " + p + " must be a decimal literal but it is: " + obj)
      }
    } yield node
  }


  /**
   *
   */
  def stringFromPredicateOptional(p: IRI): RDFParser[Option[String]] =
    optional(stringFromPredicate(p))

  def objectFromPredicateOptional(p: IRI): RDFParser[Option[RDFNode]] =
    optional(objectFromPredicate(p))

  def decimalLiteralFromPredicateOptional(p: IRI): RDFParser[Option[DecimalLiteral]] =
    optional(decimalLiteralFromPredicate(p))

  /**
   * Returns a parser that obtains the type associated with the current node
   * <p>
   * Fails if there are more than one type associated
   */
  def rdfType: RDFParser[RDFNode] = { (n, rdf) =>
    for {
      t <- objectFromPredicate(`rdf:type`)(n, rdf)
    } yield t
  }

  /**
   * Returns a parser that obtains the set of types associated
   * with the current node
   */
  def rdfTypes: RDFParser[Set[RDFNode]] =
    objectsFromPredicate(`rdf:type`)

  /**
   * RDFParser that retrieves the object associated with current node for a given predicate
   * <p>
   * Fails if there are more than one object
   *
   * @param p predicate
   */
  def objectFromPredicate(p: IRI): RDFParser[RDFNode] = { (n, rdf) => for {
    ts <- rdf.triplesWithSubjectPredicate(n, p)
    r <- ts.size match {
      case 0 => parseFail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => parseOk(ts.head.obj)
      case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
   } yield r
  }

  /**
   * RDFParser that retrieves the set of iriObjects associated with the current node for a given predicate
   * <p>
   *
   * @param p predicate
   */
  def objectsFromPredicate(p: IRI): RDFParser[Set[RDFNode]] = { (n, rdf) => for {
    triples <- rdf.triplesWithSubjectPredicate(n, p)
    r <- parseOk(objectsFromTriples(triples))
   } yield r
  }

  /**
   * A parser of the RDF List associated with the current node
   * <p>
   * Fails if there are more than one iriObjects associated with `rdf_first` or `rdf_rest`
   */
  def rdfList: RDFParser[List[RDFNode]] = { (n, rdf) =>
    n match {
      case `rdf:nil` => parseOk(List())
      case _ => for {
          elem <- objectFromPredicate(`rdf:first`)(n, rdf)
          next <- objectFromPredicate(`rdf:rest`)(n, rdf)
          ls <- rdfList(next, rdf) // TODO: Possible infinite loop if one of the nodes makes a loop
        } yield (elem :: ls)
      }
  }

  /**
   * Obtains the RDF list associated with a predicate for the current node
   *
   * @param p predicate
   */
  def rdfListForPredicate(p: IRI): RDFParser[List[RDFNode]] = { (n, rdf) =>
    for {
      value <- objectFromPredicate(p)(n, rdf)
      ls <- rdfList(value, rdf)
    } yield ls
  }

  /**
    * Obtains the RDF list associated with a predicate for the current node
    * If there is no value, returns the empty list
    *
    * @param p predicate
    */
  def rdfListForPredicateAllowingNone(p: IRI): RDFParser[List[RDFNode]] = { (n, rdf) =>
    for {
      maybeValue <- objectFromPredicateOptional(p)(n,rdf)
      ls <- maybeValue match {
        case None => parseOk(List())
        case Some(value) => rdfList(value,rdf)
      }
    } yield ls
  }

  /**
   * Obtains an integer literal associated with a predicate in the current node
   *
   * @param p predicate
   */
  def integerLiteralForPredicate(p: IRI): RDFParser[Int] = { (n, rdf) =>
    for {
      ts <- rdf.triplesWithSubjectPredicate(n, p)
      r <- ts.size match {
        case 0 => parseFail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
        case 1 => getIntegerLiteral(ts.head)
        case _ => parseFail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
      }
    } yield r
  }

  def integerLiteralsForPredicate(p: IRI): RDFParser[List[Int]] = { (n, rdf) => for {
    ts <- rdf.triplesWithSubjectPredicate(n, p)
    // val zero : Either[String,List[Int]] = Right(List())
    r <- {
      def cmb(ls: Either[String, List[Int]], node: RDFNode): Either[String, List[Int]] =
      ls.fold(e => Left(e),
                vs =>
                  node match {
                    case i: IntegerLiteral => Right(i.int :: vs)
                    case _                 => Left(s"node $node must be an integer literal")
                })
      ts.map(_.obj).foldLeft(List[Int]().asRight[String])(cmb)
    }
   } yield r
  }

  /**
   * Returns `true` if the current node does not have a given type
   *
   * @param t type to be checked
   */
  def hasNoRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)(n, rdf)
    } yield !declaredTypes.contains(t)
  }

  /**
   * Returns `true` if the current node has a given type
   *
   * @param t type to be checked
   */
  def hasRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)(n, rdf)
    } yield declaredTypes.contains(t)
  }

  /**
   * Returns `true` if the current node has a type which belong to a given set of types
   *
   * @param ts set of types to be checked
   */
  def hasSomeRDFType(ts: Set[IRI]): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(`rdf:type`)(n, rdf)
    } yield {
      val iriTypes = declaredTypes.collect { case i: IRI => i}
      iriTypes.diff(ts).size > 0
    }
  }

  /**
   * An RDF parser that parses a value of type `a` if possible
   *
   */
  def optional[A](parser: RDFParser[A]): RDFParser[Option[A]] = { (n, rdf) =>
    parser(n, rdf) match {
      case Right(v) => Right(Some(v))
      case Left(_) => Right(None)
    }
  }

  /**
   * Checks if some of the parsers pass and returns the corresponding value
   *
   * @param ps sequence of parsers
   */
  def someOf[A](ps: RDFParser[A]*): RDFParser[A] = { (n, rdf) =>
    {
      val zero: ES[A] = "someOf: none of the RDFParsers passed".asLeft[A]
      def cmb(c: ES[A], parser: RDFParser[A]): ES[A] =
        c.fold(
          _ => parser(n,rdf),
          _ => c
        )
      ps.foldLeft(zero)(cmb)
    }
  }

  /**
   * Applies a parser over a sequence of nodes
   *
   * @param parser parser
   * @param nodes sequence of nodes
   */
  def group[A](
    parser: RDFParser[A],
    nodes: Seq[RDFNode]): RDFParser[Seq[A]] = { (_, rdf) =>
    {
      val empty: Seq[A] = List()
      nodes.foldRight(parseOk(empty)) {
        case (node, s) => {
          s match {
            case Right(rs) => {
              parser(node, rdf) match {
                case Right(a) => Right(a +: rs)
                case Left(s) =>
                  parseFail("group: Unimplemented parsing node " + node + "\nMessage: " + s) // TODO
              }
            }
            case _ => s
          }
        }
      }
    }
  }

  /**
   * Applies a list of parsers
   * If a parser fails, it continues with the rest of the list
   * @return the list of successful values that can be parsed
   */
  def anyOf[A](ps: RDFParser[A]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[A]): RDFParser[Seq[A]] = (n, rdf) => {
      p(n, rdf) match {
        case Left(_) => {
          rest(n, rdf)
        }
        case Right(x) => {
          for {
            xs <- rest(n, rdf)
          } yield (x +: xs)
        }
      }
    }
    val zero: RDFParser[Seq[A]] = (n, rdf) => Right(Seq())
    ps.foldLeft(zero)(comb)
  }

  /**
  * Applies a list of parsers
    * @param ps: List of parsers. Each parser returns a list of values
    * @tparam A
    */
  def anyOfLs[A](ps: RDFParser[List[A]]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[List[A]]): RDFParser[Seq[A]] = (n, rdf) => {
      p(n, rdf) match {
        case Left(_) => {
          rest(n, rdf)
        }
        case Right(x) => {
          for {
            xs <- rest(n, rdf)
          } yield (x ++ xs)
        }
      }
    }
    val zero: RDFParser[Seq[A]] = (n, rdf) => Right(Seq())
    ps.foldLeft(zero)(comb)
  }

  /**
   * If a parser fails, it continues with the rest of the list
   * @return the result of the first parser that succeeds of failure
   *
   */
  def firstOf[A](ps: RDFParser[A]*): RDFParser[A] = {
    def comb(rest: RDFParser[A], p: RDFParser[A]): RDFParser[A] = (n, rdf) => {
      p(n, rdf) match {
        case Left(_) => rest(n, rdf)
        case Right(x) => Right(x)
      }
    }
    val zero: RDFParser[A] = (n, rdf) => parseFail("firstOf: none of the parsers succeeded")
    ps.foldLeft(zero)(comb)
  }

  /**
   * Checks that exactly one of the parsers succeeds on the current node
   *
   * @param parsers sequence of parsers
   */
  def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      val zero: ES[A] = "oneOf: none of the RDFParsers passed".asLeft[A]
      def cmb(c: ES[A], parser: RDFParser[A]): ES[A] = {
        c.fold(
          _ => parser(n,rdf),
          _ => parser(n,rdf).
            fold(
              _ => c,
              _ => "oneOf: More than one parser passes".asLeft[A])
        )
      }
      parsers.foldLeft(zero)(cmb)
    }
  }


  def subjectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(s, _, _) => s }
  }

  def objectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(_, _, o) => o }
  }

  def getIntegerLiteral(t: RDFTriple): Either[String, Int] = {
    t.obj match {
      case l: IntegerLiteral => parseOk(l.int)
      // TODO: case l: DatatypeLiteral(lexicalForm,datatype) => ...
      case _ => parseFail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }

  /**
   * Combine a sequence of RDFParsers
   *
   */
  def combineAll[A](ps: RDFParser[Seq[A]]*): RDFParser[Seq[A]] = {
    val zero: RDFParser[Seq[A]] = (_, _) => Right(Seq())
    ps.foldLeft(zero)(combine)
  }

  def combine[A](p1: RDFParser[Seq[A]], p2: RDFParser[Seq[A]]): RDFParser[Seq[A]] = (n, rdf) => {
    for {
      vs1 <- p1(n, rdf)
      vs2 <- p2(n, rdf)
    } yield {
      vs1 ++ vs2
    }
  }

  def mapRDFParser[A, B](ls: List[A], p: A => RDFParser[B]): RDFParser[List[B]] = {
    ls.map(v => p(v)).sequence[RDFParser,B]
  }

  def rdfListForPredicateOptional(p: IRI): RDFParser[List[RDFNode]] = (n, rdf) => for {
    maybeLs <- optional(rdfListForPredicate(p))(n, rdf)
  } yield maybeLs.fold(List[RDFNode]())(ls => ls)

  def literalFromPredicate(p: IRI): RDFParser[Literal] = (n, rdf) => for {
    o <- objectFromPredicate(p)(n, rdf)
    r <- asLiteral(o)(n,rdf)
  } yield r

  def asLiteral(n: RDFNode): RDFParser[Literal] = (_,_) => n match {
    case l: Literal => parseOk(l)
    case _ => parseFail(s"Expected node $n to be a literal")
  }

  def asLiterals(ls: List[RDFNode]): RDFParser[List[Literal]] = (n,rdf) =>
    sequenceEither(ls.map(asLiteral(_)(n,rdf)))

  def literalsFromPredicate(p: IRI): RDFParser[List[Literal]] = (n, rdf) => for {
    os <- objectsFromPredicate(p)(n, rdf)
    r <- asLiterals(os.toList)(n,rdf)
  } yield r

  def boolean: RDFParser[Boolean] = (n, rdf) => n match {
    case BooleanLiteral.trueLiteral => parseOk(true)
    case BooleanLiteral.falseLiteral => parseOk(false)
    case DatatypeLiteral("true", `xsd:boolean`) => parseOk(true)
    case DatatypeLiteral("false", `xsd:boolean`) => parseOk(false)
    case _ => parseFail(s"Expected boolean literal. Found $n")
  }

  def iri: RDFParser[IRI] = (n, rdf) => n match {
    case i: IRI => parseOk(i)
    case _ => parseFail(s"Expected IRI, found $n")
  }

  def integer: RDFParser[Int] = (n, rdf) => n match {
    case l: IntegerLiteral => parseOk(l.int)
    case _ => parseFail(s"Expected integer literal for node $n")
  }

  def string: RDFParser[String] = (n, rdf) => n match {
    case s: StringLiteral => parseOk(s.getLexicalForm)
    case _ => parseFail(s"Expected string literal for node $n")
  }

  def booleanFromPredicate(p: IRI): RDFParser[Boolean] = arc(p, boolean)

  def booleanFromPredicateOptional(p: IRI): RDFParser[Option[Boolean]] = (n, rdf) => {
    objectFromPredicateOptional(p)(n, rdf) match {
      case Right(None) => Right(None)
      case Right(Some(BooleanLiteral(b))) => Right(Some(b))
      case Right(Some(o)) => parseFail(s"value of $p must be a boolean literal. Obtained $o")
      case Left(e) => Left(e)
    }
  }

  def irisFromPredicate(p: IRI): RDFParser[List[IRI]] = (n, rdf) => {
    val r = objectsFromPredicate(p)(n, rdf)
    r match {
      case Right(ns) => nodes2iris(ns.toList) match {
          case Right(iris) => Right(iris)
          case Left(msg) => parseFail(msg)
        }
      case Left(f) => Left(f)
    }
  }

  def iriFromPredicateOptional(p: IRI): RDFParser[Option[IRI]] = { (n, rdf) =>
    optional(iriFromPredicate(p))(n, rdf)
  }


  def opt[A](pred: IRI, parser: RDFParser[A]): RDFParser[Option[A]] = (n, rdf) => {
    objectsFromPredicate(pred)(n, rdf) match {
      case Right(os) => os.size match {
        case 0 => Right(None)
        case 1 => parser(os.head, rdf).map(Some(_))
        case _ => parseFail(s"opt fails because $n has more than one value for pred $pred. Values: $os")
      }
      case Left(_) => Right(None)
    }
  }

  def starWithNodes[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[(RDFNode, A)]] = (n, rdf) => {
    for {
      os <- objectsFromPredicate(pred)(n, rdf).map(_.toList)
      vs <- parseNodes(os, parser)(rdf)
    } yield os zip vs
  }

  def star[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] = (n, rdf) => {
    for {
      os <- objectsFromPredicate(pred)(n, rdf).map(_.toList)
      vs <- parseNodes(os, parser)(rdf)
    } yield vs
  }

  def collect[A](ps: List[RDFParser[A]]): RDFParser[List[A]] = (n, rdf) => {
    val zero: List[A] = List()
    def combine(xs: List[A], parser: RDFParser[A]): List[A] = {
      parser(n, rdf) match {
        case Right(x) => x :: xs
        case Left(_) => xs
      }
    }
    Right(ps.foldLeft(zero)(combine))
  }

  def checkType(expected: RDFNode): RDFParser[Boolean] = (n, rdf) => for {
    obtained <- objectFromPredicate(`rdf:type`)(n, rdf)
    v <- if (obtained == expected) Right(true)
    else
      parseFail(s"Type of node $n must be $expected but obtained $obtained")
  } yield v

  def condition(cond: RDFNode => Boolean, name: String): RDFParser[RDFNode] = (n, _) =>
    if (cond(n)) Right(n)
    else parseFail(s"Condition $name not satisfied on node $n")

  def failIf(cond: Boolean, msg: String): RDFParser[Unit] = (n, _) =>
    if (cond) parseFail(s"Condition failed: $msg. Current node: $n")
    else Right(())

  def arc[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = (n, rdf) => for {
    obj <- objectFromPredicate(pred)(n, rdf)
    x <- parser(obj, rdf)
  } yield x

  /**
   * Parses a list of values. The list must contain at least two values
   */
  def list2Plus[A](p: RDFParser[A]): RDFParser[List[A]] = (n, rdf) => for {
    first <- arc(`rdf:first`, p)(n, rdf)
    restNode <- objectFromPredicate(`rdf:rest`)(n, rdf)
    rest <- list1Plus(p)(restNode, rdf)
  } yield first :: rest

  /**
   * Parses a list of values. The list must contain at least one value
   */
  def list1Plus[A](p: RDFParser[A]): RDFParser[List[A]] = list1PlusAux(p, List())

  def list1PlusAux[A](p: RDFParser[A], visited: List[RDFNode]): RDFParser[List[A]] = (n, rdf) => for {
    first <- arc(`rdf:first`, p)(n, rdf)
    restNode <- objectFromPredicate(`rdf:rest`)(n, rdf)
    rest <- parseRest(visited, restNode, p)(n, rdf)
  } yield first :: rest

  def parseRest[A](
    visited: List[RDFNode],
    restNode: RDFNode,
    parser: RDFParser[A]): RDFParser[List[A]] = (n, rdf) =>
    if (restNode == `rdf:nil`) parseOk(List[A]())
    else if (visited contains restNode)
      parseFail(s"Parsing list with recursive nodes. visitedNodes: $visited, node: $restNode")
    else
      list1PlusAux(parser, restNode :: visited)(restNode, rdf)

  def rdfNil[A]: RDFParser[List[A]] = (n, rdf) =>
    if (n == `rdf:nil`) Right(List())
    else parseFail(s"Expected rdf_nil but got $n")

  def nodes2iris(ns: List[RDFNode]): Either[String, List[IRI]] = {
    sequenceEither(ns.map(_.toIRI))
  }

  // Todo: Use "sequence" when I find why it gives a type error...
  def sequenceEither[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = {
    val zero: Either[E, List[A]] = Either.right(List())
    def next(r: Either[E, List[A]], x: Either[E, A]): Either[E, List[A]] =
      x match {
        case Left(e) => Left(e)
        case Right(v) => r match {
          case Left(e) => Left(e)
          case Right(vs) => Right(v :: vs)
        }
      }
    xs.foldLeft(zero)(next)
  }

  /*  def fromEitherString[A](e: Either[String,A]): Try[A] =
    e.fold(str => parseFail(str),v => Success(v)) */

  /* TODO: This method could be removed
  def hasPredicateWithSubject(n: RDFNode, p: IRI, rdf: RDFReader): Boolean = {
    rdf.triplesWithSubjectPredicate(n, p).size > 0
  } */

  def ok[A](x: A): RDFParser[A] = (_,_) =>
    parseOk(x)

  def parseFail[A](str: String): Either[String, A] =
    Left(str)

  def parseOk[A](x: A): Either[String, A] =
    Right(x)

  def parseNodes[A](
    nodes: List[RDFNode],
    parser: RDFParser[A]): RDFReader => Either[String, List[A]] = rdf =>
    sequenceEither(nodes.map(parser(_, rdf)))

  def parsePredicateLiteralList[A](p: IRI, maker: Literal => A): RDFParser[List[A]] = (n, rdf) => for {
    vs <- literalsFromPredicate(p)(n, rdf)
  } yield vs.map(maker(_))

  def parsePredicateLiteral[A](p: IRI, maker: Literal => A): RDFParser[A] = (n, rdf) => for {
    v <- literalFromPredicate(p)(n, rdf)
  } yield maker(v)

  def parsePredicateInt[A](p: IRI, maker: Int => A): RDFParser[A] = (n, rdf) => for {
    v <- integerLiteralForPredicate(p)(n, rdf)
  } yield maker(v.intValue())

  def parsePredicateIntList[A](p: IRI, maker: Int => A): RDFParser[List[A]] = (n, rdf) => for {
    vs <- integerLiteralsForPredicate(p)(n, rdf)
  } yield vs.map(maker(_))

  def parsePredicateString[A](p: IRI, maker: String => A): RDFParser[A] = (n, rdf) => for {
    v <- stringFromPredicate(p)(n, rdf)
  } yield maker(v)

  def parsePredicateList[A](p: IRI, maker: RDFNode => A): RDFParser[List[A]] = (n, rdf) => {
    for {
      os <- objectsFromPredicate(p)(n, rdf)
    } yield os.toList.map(o => maker(o))
  }

  def parsePredicate[A](p: IRI, maker: RDFNode => A): RDFParser[A] = (n, rdf) => {
    for {
      o <- objectFromPredicate(p)(n, rdf)
    } yield maker(o)
  }

  def parsePredicateIRI[A](p: IRI, maker: IRI => A): RDFParser[A] = (n, rdf) => for {
    iri <- iriFromPredicate(p)(n, rdf)
  } yield maker(iri)

  def parsePredicateIRIList[A](p: IRI, maker: IRI => A): RDFParser[List[A]] = (n, rdf) => for {
    iris <- irisFromPredicate(p)(n, rdf)
  } yield iris.map(maker(_))


}
