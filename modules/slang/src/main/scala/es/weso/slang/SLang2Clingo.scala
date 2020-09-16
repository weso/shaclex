package es.weso.slang
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
import es.weso.rdf.operations.Graph
import es.weso.utils.IOUtils.fromES
//import es.weso.rdf.operations.Graph
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps.{BNodeLabel => SMBNodeLabel, IRILabel => SMIRILabel, _}
import es.weso.shapeMaps.ShapeMap
import es.weso.slang.Clingo._
// import cats.syntax.either._

import scala.annotation.tailrec
import cats.effect.IO
import fs2.Stream

object ClingoNames {
  val ARC = "arc"
  val BNODE = "bNode"
  val CHECK = "check"
  val TRUE = "true"
  val IRI = "iri"
  val LITERAL = "literal"
  val LABEL = "label"
  val SCHEMA = "schema"
  val SHAPE = "shape"
  val AND = "and"
  val BNODEKIND = "bNode"
  val DATATYPE = "datatype"
  val INTMAX = "intMax"
  val IRIKIND = "iri"
  val NO = "no"
  val QUALIFIEDARC = "qa"
  val REF = "ref"
  val CARD = "card"
  val STAR = "star"
}

trait SLang2Clingo {

  import ClingoNames._

  case class ClingoException(str: String) extends RuntimeException(str)

  def validate2Clingo(smap: ShapeMap,
                      rdf: RDFReader,
                      schema: SchemaS
                     ): IO[Program] = {
    val zero: IO[Program] = IO(commonStatements(schema))
    def comb(a: Association, next: IO[Program]): IO[Program] = a.node match {
      case RDFNodeSelector(node) => for {
        lbl <- fromES(cnvLabel(a.shape))
        p1 <- ground(node,lbl,rdf,schema)
        p2 <- next
      } yield p1.append(p2)
      case _ => IO.raiseError(ClingoException(s"Unhandled association node: ${a.node}"))
    }
    smap.associations.foldRight(zero)(comb)
  }

  private def commonStatements(schema:SchemaS): Program = {

//    val showHasShape = Seq(ShowDirective(CHECK, 2))
//    val debugShows = Seq(ShowDirective("hasShape", 2))

    val hasShapeTrue = PlainString(
      """|hasShape(X,true):-node(X) .
      """.stripMargin)

/*    val hasShapeInt = PlainString(
      """|hasShape(X, int):-int(X).
         |:- hasShape(X, int), not int(X).
      """.stripMargin) */

    def hasShapeDatatype = PlainString(
      """|hasShape(X, datatype(D)):- shape(datatype(D)), literal(X,D).
         |:- hasShape(X, datatype(D)), not literal(X,D).
      """.stripMargin)

    val hasShapeIri = PlainString(
      """|hasShape(X, iri):-iri(X).
         |:- hasShape(X, iri), not iri(X).
      """.stripMargin)

    val hasShapeBNode = PlainString(
      """|hasShape(X, bNode) :- bNode(X).
         |:- hasShape(X, bNode), not bNode(X).
      """.stripMargin)

    val hasShapeQAIntMax = PlainString(
      """|hasShape(X,qa(P,S,Min,intMax(Max))):-
         | shape(qa(P,S,Min,intMax(Max))),
         | countPropShape(X,P,S,C), Min <= C, C <= Max .
         |:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
         |:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      """.stripMargin)

    val hasShapeQAStar = PlainString(
      """|hasShape(X,qa(P,S,Min,star)):-
         | shape(qa(P,S,Min,star)),
         | countPropShape(X,P,S,C), Min <= C .
         |:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      """.stripMargin)

    val hasShapeAnd = PlainString(
      """|hasShape(X,and(S1,S2)):-
         | shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
         |:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
         |:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      """.stripMargin)

    val hasShapeRef = PlainString(
      """|hasShape(X,ref(Lbl)):-
         |  shape(ref(Lbl)), schema(Lbl,S), hasShape(X,S).
         |:-hasShape(X,ref(Lbl)), schema(Lbl,S), not hasShape(X,S) .
      """.stripMargin)

    val hasShapeNot = PlainString(
      """|hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
         |:- hasShape(X, no(S)), hasShape(X,S) .
      """.stripMargin)

  def selectFragments(schema: SchemaS): Set[Statement] =
      selectFragmentsAux(schema.lblMap.values.toList, List(), Set())

  @tailrec
  def selectFragmentsAux(shapes: List[SLang], visited: List[SLang], current: Set[Statement]): Set[Statement] =
      shapes match {
      case Nil => current
      case s :: rest => if (visited contains(s)) {
        selectFragmentsAux(rest, visited, current)
      } else {
        val fragment = s match {
         case STrue => hasShapeTrue
         case _: And => hasShapeAnd
         case IRIKind => hasShapeIri
         case BNodeKind => hasShapeBNode
         case Datatype(d) => hasShapeDatatype
         case QualifiedArc(_,_,Card(_,IntMax(_))) => hasShapeQAIntMax
         case QualifiedArc(_, _, Card(_,Star)) => hasShapeQAStar
         case _: Ref => hasShapeRef
         case Not(_) => hasShapeNot
       }
       selectFragmentsAux(s.children ++ rest, s :: visited, current + fragment)
      }
    }

    Program(
      selectFragments(schema).toSeq ++
      Seq(PlainString(
        """|
           |#show result/2 .
           |result(X,Lbl) :- shapeMap(X,Lbl), schema(Lbl,S), hasShape(X,S).
           |result(X,no(Lbl)):- shapeMap(X,Lbl), schema(Lbl,S), not hasShape(X,S) .
           |
           |% Remove CWA on hasShape
           |hasShape(X,S) | not hasShape(X,S):-node(X), shape(S) .
           |
           |% #show countPropShape/4 .
           |countPropShape(X,P,S,T):-
           | node(X), pred(P), shape(S),
           | #count { V: arcWithShape(X,P,S,V) } = T .
           |
           |% #show arcWithShape/4 .
           |arcWithShape(X,P,S,V):-arc(X,P,V),hasShape(V,S).
           |
           |node(X):-arc(X,_,_).
           |node(X):-arc(_,_,X).
           |pred(P):-arc(_,P,_).
           |pred(P):-shape(qa(P,_,_,_)).
           |
          """.stripMargin)
      ))
  }

  private def cnvLabel(lbl: ShapeMapLabel): Either[String,Label] = lbl match {
    case Start => Left(s"Not supported start in clingo conversion yet")
    case SMIRILabel(iri) => Right(IRILabel(iri))
    case SMBNodeLabel(bnode) => Right(BNodeLabel(bnode))
  }

  private def ground(node: RDFNode,
             label: Label,
             rdf: RDFReader,
             schema: SchemaS): IO[Program] = 
    for {
     shape <- schema.getLabel(label).fold(
       IO.raiseError[SLang](new RuntimeException(s"Label $label not found in Schema. Available labels: ${schema.availableLabels.mkString(",")}"))
     )(s => IO.pure(s))
     rdfStatements <- groundRDF(node,rdf)
     shapeStatements = groundShape(shape)
     schemaStatements = groundSchema(schema)
     shapeMapStatements = groundShapeMap(node,label)
    } yield {
    val all = rdfStatements.statements ++ shapeStatements ++ schemaStatements ++ shapeMapStatements
    Program(all)
  }

  private  def closure(node: RDFNode, rdf: RDFReader): Stream[IO, RDFTriple] =
     rdf.triplesWithSubject(node) ++ rdf.triplesWithObject(node)

  private def triple2Statement(t: RDFTriple): Statement = {
    mkFact(ARC,node2Term(t.subj), node2Term(t.pred), node2Term(t.obj))
  }

  private def node2Statement(node: RDFNode): Statement = node match {
    case i: IRI => mkFact(IRI, node2Term(node))
    case b: BNode => mkFact(BNODE, node2Term(node))
    case l: RDFLiteral => mkFact(LITERAL, node2Term(node),iri2Term(l.dataType))
  }

  private def groundRDF(node: RDFNode, rdf: RDFReader): IO[Program] = for {
    nodes <- Graph.traverse(node,rdf).compile.toList
    triples <- Graph.traverseWithArcs(node,rdf).compile.toList
  } yield {
    val statementsNodes = nodes.map(node2Statement(_))
    val statementsTriples = triples.map(triple2Statement(_))
    // val statementsPredicates = triples.map(_.pred).distinct.map(pred2Statement(_))
    Program(statementsNodes ++ statementsTriples)
  }

  private def groundShapeMap(node: RDFNode, label: Label): List[Statement] = {
    List(mkFact("shapeMap",node2Term(node),label2Term(label)))
  }

  private def groundShape(shape: SLang): List[Statement] =
    groundShapeAux(List(shape), List(), List())

  @tailrec
  private def groundShapeAux(pending: List[SLang],
                             visited: List[SLang],
                             current: List[Statement]
                            ): List[Statement] =
    pending match {
      case Nil => current
      case shape :: rest =>
        if (visited contains(shape))
          groundShapeAux(rest, visited, current)
        else {
          groundShapeAux(shape.children ++ rest, shape :: visited, mkShape(shape) ++ current)
        }
    }

  private def mkShape(s: SLang): List[Statement] = List(mkFact(SHAPE, shape2Term(s)))

  private def groundSchema(schema: SchemaS): List[Statement] = {
    schema.lblMap.toList.map { case (label, shape) =>
      schemaLabelShape(label,shape)
    }.flatten ++
      schema.lblMap.keySet.toList.map(label2Statement(_))
  }

  private def label2Statement(lbl: Label): Statement = {
    mkFact(LABEL,label2Term(lbl))
  }

  private def schemaLabelShape(label: Label,
                               shape: SLang): List[Statement] = {
    List(mkFact(SCHEMA, label2Term(label), shape2Term(shape)))
  }

/*  private def shape2Program(s: SLang): Program =
    Program(List(mkFact(SHAPE,shape2Term(s))))

  private def shapeTrue: Statement = mkFact(SHAPE, Const(TRUE)) */


  private def shape2Term(shape: SLang): Term = shape match {
    case STrue         => Const(TRUE)
    case And(s1,s2)    => Func(AND, shape2Term(s1), shape2Term(s2))
    case BNodeKind     => Const(BNODEKIND)
    case IRIKind       => Const(IRIKIND)
    case Datatype(iri) => Func(DATATYPE, iri2Term(iri))
    case Not(s)        => Func(NO,shape2Term(s))
    case Ref(lbl) => Func(REF,label2Term(lbl))
    case QualifiedArc(ps, shape, card) =>
      Func(QUALIFIEDARC,iri2Term(getPred(ps)), shape2Term(shape), IntTerm(card.min), max2Term(card.max))
  }

  private def getPred(pp: PropPath): IRI = pp match {
    case Pred(iri) => iri
    case _ => 
     throw new Exception(s"Unsupported $pp yet")
  }

  private def max2Term(max: Max): Term =
    max match {
      case Star => Const(STAR)
      case IntMax(n) => Func(INTMAX, IntTerm(n))
    }

  private def mkFact(name: String, terms: Term*): Statement =
    Fact(Pos(Function(Func(name, terms: _*))))

  private def label2Term(label: Label): Term = node2Term(label.toRDFNode)

  private def iri2Term(i: IRI): Term = StringTerm("<" ++ i.str ++ ">")

  private def node2Term(node: RDFNode): Term = node match {
    case iri: IRI => iri2Term(iri)
    case bNode: BNode => StringTerm("_:" ++ bNode.id )
    case l: StringLiteral => StringTerm(s"${l.getLexicalForm}")
    case l: IntegerLiteral => IntTerm(l.int)
    case l: LangLiteral => StringTerm(s"${l.getLexicalForm}@${l.lang}")
    case l: RDFLiteral => StringTerm((s"${l.getLexicalForm}^^<${l.dataType.str}>"))
  }


}