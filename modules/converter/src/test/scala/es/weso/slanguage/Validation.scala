package es.weso.slanguage

import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
import es.weso.rdf.operations.Graph
import es.weso.rdf.triples.RDFTriple
import es.weso.slanguage.Clingo._

case class Value[A](m: Map[A,Val]) {

  def addValue(x: A, value: Val): Value[A] = m.get(x) match {
    case None => Value(m.updated(x,value))
    case Some(otherValue) => Value(m.updated(x, otherValue.combineVal(value)))
  }

  def combine(otherValue: Value[A]): Value[A] = {
    val zero = this
    def comb(rest: Value[A], pair:(A,Val)): Value[A] = {
      val (a,v) = pair
      rest.addValue(a,v)
    }
    otherValue.m.foldLeft(zero)(comb)
  }

  def conform(x: A): Value[A] = addValue(x,Conforms)
  def notConform(x:A): Value[A] = addValue(x,NotConforms)

  def getVal(x:A): Val = m.get(x).getOrElse(Unknown)

}

object Value {
  def conform[A](x: A): Value[A] = Value(Map(x -> Conforms))
  def notConform[A](x:A): Value[A] = Value(Map(x -> NotConforms))
}


sealed trait Val extends Product with Serializable {
  def combineVal(other: Val): Val
}
case object Conforms extends Val {
  override def combineVal(other: Val): Val = other match {
    case Conforms | Unknown => this
    case NotConforms | Inconsistent => Inconsistent
  }
}
case object NotConforms extends Val {
  override def combineVal(other: Val): Val = other match {
    case NotConforms | Unknown => this
    case Conforms | Inconsistent => Inconsistent
  }
}
case object Unknown extends Val {
  override def combineVal(other: Val): Val = other
}
case object Inconsistent extends Val {
  override def combineVal(other: Val): Val = Inconsistent
}

case class ShapesMap(map: Map[RDFNode, Value[SLang]]) {
  def conform(node: RDFNode, shape: SLang): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value.conform(shape)))
    case Some(value) => ShapesMap(map.updated(node, value.conform(shape)))
  }

  def notConform(node: RDFNode, shape: SLang): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, Value.notConform(shape)))
    case Some(value) => ShapesMap(map.updated(node, value.notConform(shape)))
  }

  def addValue(node: RDFNode, value: Value[SLang]): ShapesMap = map.get(node) match {
    case None => ShapesMap(map.updated(node, value))
    case Some(otherValue) => ShapesMap(map.updated(node, value.combine(otherValue)))
  }

  def combine(other: ShapesMap): ShapesMap = {
    val zero: ShapesMap = this
    def comb(rest: ShapesMap, pair: (RDFNode, Value[SLang])): ShapesMap = {
      val (node,value) = pair
      rest.addValue(node, value)
    }
    other.map.foldLeft(zero)(comb)
  }
}

object ShapesMap {
  def empty: ShapesMap = ShapesMap(Map())
}

object Validation {

  def validate(node: RDFNode, shape: SLang, rdf: RDFReader, smap: ShapesMap): ShapesMap =
   shape match {
    case STrue => smap.conform(node,shape)
    case And(s1,s2) => {
     val m1 = validate(node, s1, rdf, smap)
     val m2 = validate(node, s2, rdf, smap)
     m1.combine(m2)
    }
    case BNodeKind =>
     if (node.isBNode) smap.conform(node, shape)
     else smap.notConform(node,shape)
    case IRIKind =>
     if (node.isIRI) smap.conform(node, shape)
     else smap.notConform(node,shape)
    case Datatype(iri) =>
     node match {
      case l: RDFLiteral =>
       if (l.dataType == iri) smap.conform(node,shape)
       else smap.notConform(node,shape)
      case _ => smap.notConform(node,shape)
    }
    case Not(s) => ???
    case Ref(_) => ???
    case QualifiedArc(pred, shape, card) => ???
   }

  case class CLConst(s: String)

  def commonStatements: Program = {
    val showHasShape = Seq(ShowDirective("hasShape", 2))
    val debugShows = Seq(ShowDirective("arc", 3))

    Program(
        showHasShape ++
        debugShows ++
        slangStatements.statements
      )
  }

  def slangStatements: Program = {
    val s1 = hasShapeOrNot
    Program(Seq(s1))
  }

  def hasShapeOrNot: Statement = {
    val x: Var = Var("X")
    val s: Var = Var("S")
    val head: Head = Disj(
      Pos(Function(Func("hasShape", x, s))),
      Neg(Function(Func("hasShape", x, s)))
    )
    Rule(head, Pos(Function(Func("node", x))), Pos(Function(Func("shape", s))))
  }

  def ground(node: RDFNode,
             label: Label,
             rdf: RDFReader,
             schema: SchemaS): Either[String,Program] = {
    schema.getLabel(label) match {
      case Some(shape) => {
        val rdfStatements = groundRDF(node,rdf)
        val shapeStatements = groundShape(shape)
        val common = commonStatements
        val all: Seq[Statement] = rdfStatements.statements ++ shapeStatements.statements ++ common.statements
        Right(Program(all))
      }
      case None => Left(s"Shape with label $label not found in Schema")
    }
  }

  def closure(node: RDFNode, rdf: RDFReader): List[RDFTriple] =
    rdf.triplesWithSubject(node).toList ++
    rdf.triplesWithObject(node).toList

  def node2Term(node: RDFNode): Term = node match {
    case iri: IRI => iri2Term(iri)
    case bNode: BNode => StringTerm("_:" ++ bNode.id )
    case l: StringLiteral => StringTerm(s"${l.getLexicalForm}")
    case l: IntegerLiteral => IntTerm(l.int)
    case l: LangLiteral => StringTerm(s"${l.getLexicalForm}@${l.lang}")
    case l: RDFLiteral => StringTerm((s"${l.getLexicalForm}^^<${l.dataType.str}>"))
  }

  def triple2Statement(t: RDFTriple): Statement = {
    Fact(Pos(Function(
      Func("arc",
      node2Term(t.subj), node2Term(t.pred), node2Term(t.obj))))
    )
  }

  def node2Statement(node: RDFNode): Statement = node match {
    case i: IRI => mkFact("iri", node2Term(node))
    case b: BNode => mkFact("bNode", node2Term(node))
    case l: RDFLiteral => mkFact("literal", node2Term(node),iri2Term(l.dataType))
  }

  def pred2Statement(pred: IRI): Statement =
    mkFact("pred", iri2Term(pred))

  def iri2Term(i: IRI): Term = StringTerm("<" ++ i.str ++ ">")

  def mkFact(name: String, terms: Term*): Statement =
    Fact(Pos(Function(Func(name, terms: _*))))

  def groundRDF(node: RDFNode, rdf: RDFReader): Program = {
    val (nodes,triples) = Graph.traverseWithArcs(node,rdf)
    val statementsNodes = nodes.map(node2Statement(_))
    val statementsTriples = triples.map(triple2Statement(_))
    val statementsPredicates = triples.map(_.pred).distinct.map(pred2Statement(_))
    Program(
      statementsNodes ++
      statementsTriples ++
      statementsPredicates
    )
  }

  def groundShape(shape: SLang): Program = shape match {
    case STrue => shape2Program(shape)
    case And(s1, s2) => {
       Program(
        shape2Program(shape).statements ++
        groundShape(s1).statements ++
        groundShape(s2).statements
      )
    }
    case _: Ref => shape2Program(shape)
    case _: Datatype => shape2Program(shape)
    case IRIKind | BNodeKind => shape2Program(shape)
    case Not(s) =>
      Program(
        shape2Program(shape).statements ++
        groundShape(s).statements
      )
    case QualifiedArc(_, shape, _) =>
      Program(
        shape2Program(shape).statements ++
        groundShape(shape).statements
      )
  }

  private def shape2Program(shape: SLang): Program =
    Program(List(Fact(Pos(Function(Func("shape",shape2Term(shape)))))))

  private def shape2Term(shape: SLang): Term = shape match {
    case STrue => Const("true")
    case And(s1,s2) => Func("and", shape2Term(s1), shape2Term(s2))
    case BNodeKind => Const("bnodeKind")
    case IRIKind => Const("iriKind")
    case Datatype(iri) => Func("datatype", StringTerm(iri.str))
    case Not(s) => Func("not",shape2Term(s))
    case Ref(Label(lbl)) => Func("ref",StringTerm(lbl.str))
    case QualifiedArc(pred, shape, card) =>
      Func("qualifiedArc",StringTerm(pred.str), shape2Term(shape), card2Term(card))
  }

  def card2Term(card: Card): Term =
    Func("card", IntTerm(card.min), card.max match {
      case Star => Const("star")
      case IntMax(n) => IntTerm(n)
    })

}