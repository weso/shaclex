package es.weso.shacl

import scala.util.{ Failure, Success, Try }

import org.slf4s.Logging

import es.weso.rdf.{ PrefixMap, RDFReader }
import es.weso.rdf.nodes.{ BNodeId, IRI, Literal, RDFNode }
import es.weso.rdf.parser.RDFParser
import es.weso.utils.TryUtils._

import SHACLPrefixes._

object RDF2Shacl 
    extends Logging
    with RDFParser {
  
  /**
   * Parses RDF content and obtains a SHACL Schema and a PrefixMap 
   */
  def getShacl(rdf: RDFReader): Try[(Schema, PrefixMap)] = {
    val pm = rdf.getPrefixMap  
    for {
      shapes <- shapes(rdf)
    } yield (Schema(shapes), pm)
  }
  
  def shapes(rdf: RDFReader): Try[Seq[Shape]] = {
   val shape_nodes = subjectsWithType(sh_Shape, rdf)
   filterSuccess(shape_nodes.toSeq.map (node => shape(node,rdf)))
  }
  
  def shape(node: RDFNode, rdf: RDFReader): Try[Shape] = {
    node match {
      case iri : IRI => {
        for {
          scopes <- scopes(node,rdf)
          filters <- filters(node,rdf)
          constraints <- constraints(node,rdf)
        } yield Shape(Some(iri),scopes, filters, constraints)
      }
      case _ => fail("Non supported shapes without IRI Id. Node: " + node)
    }
  }
  
  def scopes: RDFParser[Seq[Scope]] = 
    combineAll(
          scopeNodes
        // TODO: Add the rest of scope declarations
        //  , scopeClass
        )
    
  def scopeNodes: RDFParser[Seq[Scope]] = (n,rdf) => {
    val attempts = for {
      ns <- objectsFromPredicate(sh_scopeNode)(n,rdf)
    } yield {
      val xs = ns.toSeq.map(mkScopeNode)
      filterSuccess(xs)
    }
    attempts.flatten
  }
  
  def mkScopeNode(n: RDFNode): Try[ScopeNode] = {
    n match {
      case iri: IRI => Success(ScopeNode(iri))
      case _ => fail("Node " + n + " must be an IRI to be a scope node")
    }
  }
    
  def filters: RDFParser[Seq[Shape]] = (n,rdf) => {
    // Todo add support for sh:filter
    Success(Seq())
  }
  
  def constraints: RDFParser[Seq[Constraint]] = {
    combineAll(propertyConstraints)
  }
  
  def propertyConstraints: RDFParser[Seq[Constraint]] = (n,rdf) => {
    val attempts = for {
      ps <- objectsFromPredicate(sh_property)(n,rdf)
    } yield {
      val xs = ps.toSeq.map(p => propertyConstraint(p,rdf))
      filterSuccess(xs)
    }
    attempts.flatten
  }
  
  def propertyConstraint: RDFParser[PropertyConstraint] = (n,rdf) => {
    val id = if (n.isIRI) Some(n.toIRI) else None
    for {
      predicate <- iriFromPredicate(sh_predicate)(n,rdf)
      components <- propertyConstraintComponents(n,rdf)
    } yield {
      PropertyConstraint(id, predicate, components)
    }
  }
  
  
  def propertyConstraintComponents: RDFParser[Seq[PCComponent]] = 
    allOf(minCount, maxCount, nodeKind)
  
  
  def minCount: RDFParser[PCComponent] = (n,rdf) => {
    for {
     v <- integerLiteralForPredicate(sh_minCount)(n,rdf)
    } yield {
      MinCount(v)
    }
  }
  
  def maxCount: RDFParser[PCComponent] = (n,rdf) => {
    for {
     v <- integerLiteralForPredicate(sh_maxCount)(n,rdf)
    } yield {
      MaxCount(v)
    }
  }
  
  def nodeKind: RDFParser[PCComponent] = (n,rdf) => {
    for {
      os <- objectsFromPredicate(sh_nodeKind)(n,rdf)
      nk <- parseNodeKind(os)
    } yield nk
  }
  
  def parseNodeKind(os: Set[RDFNode]): Try[PCComponent] = {
    os.size match {
      case 0 => fail("no objects of nodeKind property")
      case 1 => {
        os.head match {
          case `sh_IRI` => Success(NodeKind(IRIKind))
          case `sh_BlankNode` => Success(NodeKind(BlankNodeKind))
          case `sh_Literal` => Success(NodeKind(LiteralKind))
          case `sh_BlankNodeOrLiteral` => Success(NodeKind(BlankNodeOrLiteral))
          case `sh_BlankNodeOrIRI` => Success(NodeKind(BlankNodeOrIRI))
          case `sh_IRIOrLiteral` => Success(NodeKind(IRIOrLiteral))
          case x => fail(s"incorrect value of nodeKind property $x") 
        }
      }
      case n => fail(s"objects of nodeKind property > 1. $os") 
    }
  }

  /**
   * Applies a list of parsers 
   * If a parser fails, it continues with the rest of the list
   * @return the list of successful values that could be parsed
   * 
   */
  def allOf[A](ps:RDFParser[A]*): RDFParser[Seq[A]] = {
    def comb(rest: RDFParser[Seq[A]], p: RDFParser[A]): RDFParser[Seq[A]] = (n,rdf) => {
      p(n,rdf) match {
        case Failure(_) => rest(n,rdf)
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

  
  // TODO: Move these methods to SRDF project
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
  
  def fail(str: String) = 
    Failure(throw new Exception(str))
    
  def noScopes: Seq[Scope] = Seq()
  def noFilters: Seq[Shape] = Seq()
  def noConstraints: Seq[Constraint] = Seq()

}
