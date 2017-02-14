package es.weso.rdf.parser

import es.weso.rdf.nodes._
import es.weso.rdf._

import scala.util._
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.triples.RDFTriple
import cats._
import cats.data._
import cats.implicits._


/**
 * Exceptions raised by the RDFParser
 */
case class RDFParserException(msg: String)
  extends Exception(s"RDFParserException: " + msg)

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
  type RDFParser[a] = (RDFNode, RDFReader) => Try[a]

  /*
   * returns an RDFParser which returns the IRI associated with a predicate
   * @param p predicate
   */
  def iriFromPredicate(p: IRI): RDFParser[IRI] = { (n, rdf) =>
    for {
      node <- objectFromPredicate(p)(n, rdf)
    } yield node match {
      case i: IRI => i
      case _ => throw RDFParserException("Value of predicate " + p + " must be a IRI but it is: " + node)
    }
  }


  /**
   * Returns the String associated with a predicate `p`
   * @param p predicate
   * @return An RDFParser that returns the String associate with that predicate
   * 
   */
  def stringFromPredicate(p:IRI): RDFParser[String] = { (n,rdf) =>
    for {
      obj <- objectFromPredicate(p)(n,rdf)
    } yield {
      obj match {
        case StringLiteral(str) => str
        case _ => throw RDFParserException("Value of predicate " + p + " must be a string literal but it is: " + obj)
      }
    }
  }
  
  /**
   * 
   */
  def stringFromPredicateOptional(p: IRI): RDFParser[Option[String]] = 
    optional(stringFromPredicate(p))

  def objectFromPredicateOptional(p: IRI): RDFParser[Option[RDFNode]] = 
    optional(objectFromPredicate(p))
  
    
  /**
   * Returns a parser that obtains the type associated with the current node
   * <p>
   * Fails if there are more than one type associated
   */
  def rdfType: RDFParser[RDFNode] = { (n,rdf) =>
    for {
      ts <- objectsFromPredicate(rdf_type)(n,rdf)
    } yield 
    if (ts.size == 1) 
      ts.head
    else if (ts.size== 0)
      throw RDFParserException("Node " + n + " has no type")
    else 
      throw RDFParserException("Type for node " + n + " is not single. Found types = " + ts)
  } 
    
    
  /**
   * Returns a parser that obtains the set of types associated 
   * with the current node
   */
  def rdfTypes: RDFParser[Set[RDFNode]] =  
    objectsFromPredicate(rdf_type)

  /**
   * RDFParser that retrieves the object associated with current node for a given predicate
   * <p>
   * Fails if there are more than one object
   * 
   * @param p predicate 
   */
  def objectFromPredicate(p: IRI): RDFParser[RDFNode] = { (n, rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => parseFail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => Success(ts.head.obj)
      case _ => parseFail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
  }

  /**
   * RDFParser that retrieves the set of objects associated with the current node for a given predicate
   * <p>
   * 
   * @param p predicate 
   */
  def objectsFromPredicate(p: IRI): RDFParser[Set[RDFNode]] = { (n, rdf) =>
    val triples = rdf.triplesWithSubjectPredicate(n, p)
    Success(objectsFromTriples(triples))
  }

  /**
   * A parser of the RDF List associated with the current node 
   * <p>
   * Fails if there are more than one objects associated with `rdf_first` or `rdf_rest`
   */
  def rdfList: RDFParser[List[RDFNode]] = { (n, rdf) =>
    n match {
      case `rdf_nil` => Success(List())
      case x => {
        for {
          elem <- objectFromPredicate(rdf_first)(n, rdf)
          next <- objectFromPredicate(rdf_rest)(n, rdf)
          ls <- rdfList(next, rdf)  // TODO: Possible infinite loop if one of the nodes makes a loop
        } yield (elem :: ls)
      }
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
   * Obtains an integer literal associated with a predicate in the current node
   * 
   * @param p predicate
   */
  def integerLiteralForPredicate(p: IRI): RDFParser[Integer] = { (n, rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => parseFail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => getIntegerLiteral(ts.head)
      case _ => parseFail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
  }


  /**
   * Returns `true` if the current node does not have a given type
   * 
   * @param t type to be checked
   */
  def hasNoRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield !declaredTypes.contains(t)
  }

  /**
   * Returns `true` if the current node has a given type
   * 
   * @param t type to be checked
   */
  def hasRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield declaredTypes.contains(t)
  }

  /**
   * Returns `true` if the current node has a type which belong to a given set of types
   * 
   * @param ts set of types to be checked
   */
  def hasSomeRDFType(ts: Set[IRI]): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield (declaredTypes.map(_.toIRI).diff(ts)).size > 0
  }

  /**
   * An RDF parser that parses a value of type `a` if possible
   * 
   */
  def optional[A](parser:RDFParser[A]): RDFParser[Option[A]] = { (n,rdf) =>
    parser(n,rdf) match {
      case Success(v) => Success(Some(v))
      case Failure(e) => Success(None)
    }
  }
  
  /**
   * Checks if some of the parsers pass and returns the corresponding value
   * 
   * @param ps sequence of parsers
   */
  def someOf[A](ps: RDFParser[A]*): RDFParser[A] = { (n, rdf) =>
    {
      ps.foldLeft(parseFail("someOf: none of the RDFParsers passed")) {
        case ((s: Try[A], parser)) =>
          s match {
            case Success(_) => s
            case Failure(_) => parser(n, rdf)
          }
      }
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
      nodes: Seq[RDFNode]
   ): RDFParser[Seq[A]] = { (_, rdf) =>
    {
      val empty: Seq[A] = List()
      nodes.foldRight(Success(empty)) {
        case (node, s) => {
          s match {
            case Success(rs) => {
              parser(node, rdf) match {
                case Success(a) => Success(a +: rs)
                case Failure(f) => 
                  throw RDFParserException("group: Unimplemented parsing node " + node + "\nException: " + f.getMessage) // TODO
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
   * @return the list of successful values that could be parsed
   *
   */
  def anyOf[A](ps:RDFParser[A]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[A]): RDFParser[Seq[A]] = (n,rdf) => {
      p(n,rdf) match {
        case Failure(_) => {
          rest(n,rdf)
        }
        case Success(x) => {
          for {
            xs <- rest(n,rdf)
          } yield (x +: xs)
        }
      }
    }
    val zero : RDFParser[Seq[A]] = (n,rdf) => Success(Seq())
    ps.foldLeft(zero)(comb)
  }

  /**
   * If a parser fails, it continues with the rest of the list
   * @return the result of the first parser that succeeds of failure
   *
   */
  def firstOf[A](ps:RDFParser[A]*): RDFParser[A] = {
    def comb(rest: RDFParser[A], p: RDFParser[A]): RDFParser[A] = (n,rdf) => {
      p(n,rdf) match {
        case Failure(_) => rest(n,rdf)
        case Success(x) => Success(x)
      }
    }
    val zero : RDFParser[A] = (n,rdf) => parseFail("firstOf: none of the parsers succeeded")
    ps.foldLeft(zero)(comb)
  }

  /**
   * Checks that exactly one of the parsers succeeds on the current node
   * 
   * @param parsers sequence of parsers 
   */
  def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      val r = parsers.foldLeft(parseFail("oneOf: none of the RDFParsers passed")) {
        case ((current: Try[A], parser)) =>
          current match {
            case Success(_) => {
              parser(n, rdf) match {
                case Success(_) => parseFail("oneOf: More than one parser passes")
                case Failure(_) => current
              }
            }
            case Failure(e) => {
             parser(n, rdf) 
            }
          }
      }
      r
    }
  }

 

  // TODO: Move the following methods to some utils place
  def subjectsWithType(t: RDFNode, rdf: RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicateObject(rdf_type, t))
  }

  def subjectsWithProperty(pred: IRI, rdf: RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicate(pred))
  }

  def subjectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(s, _, _) => s }
  }

  def objectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(_, _, o) => o }
  }

 def getIntegerLiteral(t: RDFTriple): Try[Integer] = {
    t.obj match {
      case l: IntegerLiteral => Success(l.int)
      // TODO: case l: DatatypeLiteral(lexicalForm,datatype) => ...
      case _                 => parseFail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }

/**
 * Combine a sequence of RDFParsers
 *
 */
  def combineAll[A](ps: RDFParser[Seq[A]]*): RDFParser[Seq[A]] = {
    val zero : RDFParser[Seq[A]] = (_,_) => Success(Seq())
    ps.foldLeft(zero)(combine)
  }

  def combine[A](p1: RDFParser[Seq[A]], p2: RDFParser[Seq[A]]): RDFParser[Seq[A]] = (n,rdf) => {
    for {
      vs1 <- p1(n,rdf)
      vs2 <- p2(n,rdf)
    } yield {
      vs1 ++ vs2
    }
  }

  def rdfListForPredicateOptional(p: IRI): RDFParser[List[RDFNode]] = (n,rdf) => for {
    maybeLs <- optional(rdfListForPredicate(p))(n,rdf)
  } yield maybeLs.fold(List[RDFNode]())(ls => ls)

  def literalFromPredicate(p: IRI): RDFParser[Literal] = (n,rdf) => for {
    o <- objectFromPredicate(p)(n,rdf)
    r <- o match {
      case l: Literal => Success(l)
      case _ => parseFail("Value of predicate must be a literal")
    }
  } yield r

  def booleanFromPredicate(p: IRI): RDFParser[Boolean] = (n,rdf) => for {
    lit <- literalFromPredicate(p)(n,rdf)
    b <- lit match {
      case BooleanLiteral(bool) => Success(bool)
      case _ => parseFail(s"Expected boolean for predicate $p but obtained $lit")
    }
  } yield b

  def booleanFromPredicateOptional(p: IRI): RDFParser[Option[Boolean]] = (n,rdf) => {
    objectFromPredicateOptional(p)(n,rdf) match {
      case Success(None) => Success(None)
      case Success(Some(BooleanLiteral(b))) => Success(Some(b))
      case Success(Some(o)) => parseFail(s"value of $p must be a boolean literal. Obtained $o")
      case Failure(e) => Failure(e)
    }
  }

  def irisFromPredicate(p: IRI): RDFParser[List[IRI]] = (n,rdf) => {
    val r = objectsFromPredicate(p)(n,rdf)
    r match {
      case Success(ns) => {
        nodes2iris(ns.toList) match {
          case Right(iris) => Success(iris)
          case Left(msg) => parseFail(msg)
        }
      }
      case Failure(f) => Failure(f)
    }
  }

  def opt[A](pred: IRI, parser: RDFParser[A]): RDFParser[Option[A]] = (n,rdf) => {
    objectsFromPredicate(pred)(n,rdf) match {
      case Success(os) => os.size match {
        case 0 => Success(None)
        case 1 => parser(os.head,rdf).map(Some(_))
        case _ => parseFail(s"opt fails because $n has more than one value for pred $pred. Values: $os")
      }
      case Failure(e) => Success(None)
    }
  }

  def starWithNodes[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[(RDFNode, A)]] = (n,rdf) => {
    for {
      os <- objectsFromPredicate(pred)(n,rdf).map(_.toList)
      vs : List[(RDFNode,A)] <- os.map(node => parser(node,rdf).map(v => (node,v))).sequence
    } yield vs
  }

  def star[A](pred: IRI, parser: RDFParser[A]): RDFParser[List[A]] = (n,rdf) =>
    for {
      os <- objectsFromPredicate(pred)(n,rdf).map(_.toList)
      vs : List[A] <- os.map(node => parser(node,rdf)).sequence
    } yield vs

  def checkType(expected: RDFNode): RDFParser[Boolean] = (n,rdf) => for {
    obtained <- objectFromPredicate(rdf_type)(n,rdf)
    v <- if (obtained == expected) Success(true)
    else
      parseFail(s"Type of node $n must be $expected but obtained $obtained")
  } yield v

  def condition(cond: RDFNode => Boolean, name: String): RDFParser[RDFNode] = (n,_) =>
    if (cond(n)) Success(n)
    else parseFail(s"Condition $name not satisfied on node $n")

  def arc[A](pred: IRI, parser: RDFParser[A]): RDFParser[A] = (n,rdf) => for {
    obj <- objectFromPredicate(pred)(n,rdf)
    x <- parser(obj,rdf)
  } yield x

  /**
  * Parses a list of values. The list must contain at least two values
  */
  def list2Plus[A](p:RDFParser[A]): RDFParser[List[A]] = (n,rdf) => for {
    first <- arc(rdf_first,p)(n,rdf)
    rest <- list1Plus(p)(n,rdf)
  } yield first :: rest

  /**
  * Parses a list of values. The list must contain at least one value
  */
  def list1Plus[A](p: RDFParser[A]): RDFParser[List[A]] = list1PlusAux(p,List())

  def list1PlusAux[A](p:RDFParser[A], visited: List[RDFNode]): RDFParser[List[A]] = (n,rdf) => for {
    first <- arc(rdf_first, p)(n,rdf)
    restNode <- objectFromPredicate(rdf_rest)(n,rdf)
    rest <- if (restNode == rdf_nil) Success(List())
     else if (visited contains restNode)
      parseFail(s"list1Plus error parsing list with nodes pointing to itself. visitedNodes: $visited, node: $restNode")
     else list1PlusAux(p,restNode :: visited)(restNode,rdf)
  } yield first :: rest

  def rdfNil[A]: RDFParser[List[A]] = (n,rdf) =>
    if (n == rdf_nil) Success(List())
    else parseFail(s"Expected rdf_nil but got $n")

  def nodes2iris(ns: List[RDFNode]): Either[String, List[IRI]] = {
    sequenceEither(ns.map(node2IRI(_)))
  }

  // Todo: Use "sequence" when I find why it gives a type error...
  def sequenceEither[E,A](xs: List[Either[E,A]]): Either[E,List[A]] = {
    val zero: Either[E,List[A]] = Either.right(List())
    def next(r: Either[E,List[A]], x: Either[E,A]): Either[E,List[A]] =
      x match {
        case Left(e) => Left(e)
        case Right(v) => r match {
          case Left(e) => Left(e)
          case Right(vs) => Right(v :: vs)
        }
      }
    xs.foldLeft(zero)(next)
  }

  def node2IRI(node: RDFNode): Either[String,IRI] = node match {
    case (i: IRI) => Right(i)
    case _ => Left(s"$node is not an IRI\n")
  }

  def fromEitherString[A](e: Either[String,A]): Try[A] =
    e.fold(str => parseFail(str),v => Success(v))


  // TODO: This method could be removed
 def hasPredicateWithSubject(n: RDFNode, p: IRI, rdf: RDFReader): Boolean = {
    rdf.triplesWithSubjectPredicate(n, p).size > 0
  }

  def parseFail[A](str: String): Try[A] =
    Failure(new Exception(str))


}