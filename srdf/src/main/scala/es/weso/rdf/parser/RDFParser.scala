package es.weso.rdf.parser

import es.weso.rdf.nodes._
import es.weso.rdf._
import scala.util._
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.triples.RDFTriple

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
   * An RDFParser of values of type `a` takes an pointed node `RDFNode`
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
      case 0 => fail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => Success(ts.head.obj)
      case _ => fail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
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
      case 0 => fail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => getIntegerLiteral(ts.head)
      case _ => fail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
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
  def someOf[A](ps: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      ps.foldLeft(fail("someOf: none of the RDFParsers passed")) {
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
   ): RDFParser[Seq[A]] = { (n, rdf) =>
    {
      val empty: Seq[A] = List()
      nodes.foldLeft(Success(empty)) {
        case (s, node) => {
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
   * Checks that exactly one of the parsers succeeds on the current node
   * 
   * @param parsers sequence of parsers 
   */
  def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      val r = parsers.foldLeft(fail("oneOf: none of the RDFParsers passed")) {
        case ((current: Try[A], parser)) =>
          current match {
            case Success(_) => {
              parser(n, rdf) match {
                case Success(_) => fail("oneOf: More than one parser passes")
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

 
  private def fail[A](str: String): Try[A] = {
    Failure(RDFParserException(str))
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
      case _                 => fail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }

 // TODO: This method could be removed
 def hasPredicateWithSubject(n: RDFNode, p: IRI, rdf: RDFReader): Boolean = {
    rdf.triplesWithSubjectPredicate(n, p).size > 0
  }

}