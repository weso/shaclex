package es.weso.slanguage

import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
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
    // case Not(s) => if (smap.)
   }

  case class CLConst(s: String)

  def commonStatements: Program =
    Program(List(
      ShowDirective("hasShape", 2),
      ShowDirective("arc", 3)
    ))

  def ground(node: RDFNode, shape: SLang, rdf: RDFReader): Program = {
    val rdfStatements = groundRDF(node,rdf)
    val shapeStatements = groundShape(shape)
    val common = commonStatements
    Program(rdfStatements.statements ++ shapeStatements.statements ++ common.statements)
  }

  def closure(node: RDFNode, rdf: RDFReader): List[RDFTriple] =
    rdf.triplesWithSubject(node).toList ++ rdf.triplesWithObject(node).toList

  def node2Prolog(node: RDFNode): Term = node match {
    case iri: IRI => StringTerm("<" ++ iri.str ++ ">")
    case bNode: BNode => StringTerm("_:" ++ bNode.id )
    case l: StringLiteral => StringTerm(s"${l.getLexicalForm}")
    case l: IntegerLiteral => IntTerm(l.int)
    case l: LangLiteral => StringTerm(s"${l.getLexicalForm}@${l.lang}")
    case l: RDFLiteral => StringTerm((s"${l.getLexicalForm}^^<${l.dataType.str}>"))
  }

  def triple2Prolog(t: RDFTriple): Statement = {
    Fact(Pos(Function(
      Func("arc",
      List(node2Prolog(t.subj),node2Prolog(t.pred),node2Prolog(t.obj)))))
    )
  }

  def groundRDF(node: RDFNode, rdf: RDFReader): Program = {
    val triples = closure(node,rdf)
    Program(triples.map(triple2Prolog(_)))
  }
  def groundShape(shape: SLang): Program = shape match {
    case STrue => Program(List(Fact(Pos(Function(Func("shape", List(Const("true"))))))))
    case And(s1, s2) => {
      val p1 = groundShape(s1)
      val p2 = groundShape(s2)
      Program(
        List(
          Rule(Pos(Function(Func("shape", List(Func("and",List(Var("S1"), Var("S2"))))))),
            List(
              Pos(Function(Func("shape", List(Var("S1"))))),
              Pos(Function(Func("shape", List(Var("S2")))))
            )
          )) ++
          p1.statements ++
          p2.statements
      )
    }
  }

}