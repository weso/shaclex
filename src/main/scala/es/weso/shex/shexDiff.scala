package es.weso.shex
import cats._, cats.data._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._

object ShExDiff {

  type Diff[A] = ValidatedNel[String,(A,A)]

  def ok[A](x: (A,A)): Diff[A] =
    Validated.valid(x)

  def err[A](msg: String): Diff[A] =
    Validated.invalidNel(msg)

  def diff(s1: Schema, s2: Schema): Diff[Schema] = {
   val db = diffBase(s1.base,s2.base) 
   val ds = diffStartActs(s1.startActs,s2.startActs)
   (db |@| ds).map((_,_) => (s1,s2))
  }

  def diffBase(b1: Option[IRI], b2: Option[IRI]): Diff[Option[IRI]] =
    optDiff(b1,b2,iriDiff)

  def diffStartActs(s1: Option[List[SemAct]], s2: Option[List[SemAct]]):
      Diff[Option[List[SemAct]]] =
    optDiff(s1,s2,semActsDiff)

def semActsDiff(ls1: List[SemAct], ls2: List[SemAct]):
      Diff[List[SemAct]] =
  listDiff(ls1,ls2,semActDiff)

  def semActDiff(s1: SemAct, s2: SemAct): Diff[SemAct] =
    (iriDiff(s1.name, s2.name) |@|
     valueDiff(s1.code, s2.code)).map((_,_) => (s1,s2))

  def listDiff[A](
    ls1: List[A],
    ls2: List[A],
    cmp: (A,A) => Diff[A]): Diff[List[A]] = {
    val ps = ls1.zip(ls2)
    val zero : Diff[List[A]] = ok((Nil,Nil))
    def comb(rest: Diff[List[A]], p: (A,A)): Diff[List[A]] = {
      (cmp(p._1,p._2) |@| rest).map((x,xs) => (x._1 :: xs._1, x._2 :: xs._2))
    }
    ps.foldLeft(zero)(comb)
  }
   
  def valueDiff[A](x: A, y: A): Diff[A] =
    if (x == y) ok((x,y))
    else err(s"$x != $y")


  def iriDiff(i1: IRI, i2: IRI): Diff[IRI] =
    if (i1 == i2) ok((i1,i2))
    else err(s"$i1 != $i2")

  def optDiff[A](
    x1: Option[A],
    x2: Option[A],
    f: (A,A) => Diff[A]): Diff[Option[A]] = (x1,x2) match {
    case (None,None) => ok((x1,x2))
    case (None,Some(v)) => err(s"1st value is None while 2nd value is $v")
    case (Some(v),None) => err(s"1st value $v while 2nd value is None")
    case (Some(v1),Some(v2)) => f(v1,v2).map(p => (Some(p._1),Some(p._2)))
  }


}
