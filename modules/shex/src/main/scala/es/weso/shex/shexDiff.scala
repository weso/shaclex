package es.weso.shex
import cats._, cats.data._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf._

object ShExDiff {

  type Result[A] = ValidatedNel[String, ((A, A))]
  type Diff[A] = (A, A) => Result[A]

  implicit val applyResult: Apply[Result] = new Apply[Result] {
    def ap[A, B](f: Result[A => B])(fa: Result[A]): Result[B] = {
      f match {
        case Validated.Invalid(s) => Validated.Invalid(s)
        case Validated.Valid((f1, f2)) => fa match {
          case Validated.Invalid(s) => Validated.Invalid(s)
          case Validated.Valid((x1, x2)) => Validated.Valid((f1(x1), f2(x2)))
        }
      }
    }
    def map[A, B](fa: Result[A])(f: A => B): Result[B] = {
      def ff(x: (A, A)): (B, B) = (f(x._1), f(x._2))
      fa.map(ff)
    }
  }

  def ok[A](x: (A, A)): Result[A] =
    Validated.valid((x))

  def err[A](msg: String): Result[A] =
    Validated.invalidNel(msg)

  def schemaDiff: Diff[Schema] = (s1, s2) => {
    val pm: Result[Option[PrefixMap]] = prefixesDiff(s1.prefixes, s2.prefixes)
    val db: Result[Option[IRI]] = baseDiff(s1.base, s2.base)
    val ds: Result[Option[List[SemAct]]] = startActsDiff(s1.startActs, s2.startActs)

    /*    type R1[A] = ValidatedNel[String,A]
    val i1 = implicitly[Applicative[R1]]

    type R2[A] = ValidatedNel[String,(A,A)]
    val i2 = implicitly[Applicative[R2]]

    val i = implicitly[Applicative[Result]] */
    // def f: (Option[PrefixMap], Option[IRI],Option[List[SemAct]]) => Schema = ???
    Apply[Result].map3(pm, db, ds)(mkSchema)
    //    Applicative[Result[Schema]].map3( (pm,db,ds) => ((s1,s2)))
    //    (pm, db, ds).mapN((_, _, _) => ((s1, s2)))

  }

  def mkSchema(
    pm: Option[PrefixMap],
    db: Option[IRI],
    ds: Option[List[SemAct]]): Schema = Schema(pm, db, ds, None, None)

  def prefixesDiff: Diff[Option[PrefixMap]] =
    optDiff(prefixMapDiff)

  def prefixMapDiff: Diff[PrefixMap] = (pm1, pm2) =>
    mapDiff(prefixDiff, iriDiff)(pm1.pm, pm2.pm).map(_ => (pm1, pm2))

  def mapDiff[A, B](
    diffA: Diff[A],
    diffB: Diff[B]): Diff[Map[A, B]] = (m1, m2) =>
    listDiff(pairDiff(diffA, diffB))(m1.toList, m2.toList).map(_ => (m1, m2))

  def pairDiff[A, B](
    diffA: Diff[A],
    diffB: Diff[B]): Diff[(A, B)] = (p1, p2) => {
    def f(x: A, y: B): (A, B) = (x, y)
    Apply[Result].map2(diffA(p1._1, p2._1), diffB(p1._2, p2._2))(f)
  }

  //    (diffA(p1._1, p2._1), diffB(p1._2, p2._2)).map2((_, _) => ))

  def prefixDiff: Diff[Prefix] = (p1, p2) =>
    valueDiff(p1.str, p2.str).map(_ => (p1, p2))

  def baseDiff: Diff[Option[IRI]] =
    optDiff(iriDiff)

  def startActsDiff: Diff[Option[List[SemAct]]] =
    optDiff(semActsDiff)

  def semActsDiff: Diff[List[SemAct]] =
    listDiff(semActDiff)

  def semActDiff: Diff[SemAct] = (s1, s2) => {
    def f(name: IRI, code: Option[String]): SemAct = SemAct(name, code)
    Apply[Result].map2(iriDiff(s1.name, s2.name), valueDiff(s1.code, s2.code))(f)
  }
  //    (iriDiff(s1.name, s2.name), valueDiff(s1.code, s2.code)).mapN((_, _) => (s1, s2))

  def listDiff[A](cmp: Diff[A]): Diff[List[A]] = (ls1, ls2) => {
    val ps = ls1.zip(ls2)
    val zero: Result[List[A]] = ok((Nil, Nil))
    def comb(rest: Result[List[A]], p: (A, A)): Result[List[A]] = {
      def f(xs: List[A]): List[A] = p._1 :: xs // and p._2 ???
      Apply[Result].map(rest)(f)
      // (cmp(p._1, p._2), rest).mapN((x, xs) => (x._1 :: xs._1, x._2 :: xs._2))
    }
    ps.foldLeft(zero)(comb)
  }

  def valueDiff[A]: Diff[A] = (x, y) =>
    if (x == y) ok((x, y))
    else err(s"$x != $y")

  def iriDiff: Diff[IRI] = (i1, i2) =>
    if (i1 == i2) ok((i1, i2))
    else err(s"$i1 != $i2")

  def optDiff[A](f: Diff[A]): Diff[Option[A]] = (x1, x2) =>
    (x1, x2) match {
      case (None, None) => ok((x1, x2))
      case (None, Some(v)) => err(s"1st value is None while 2nd value is $v")
      case (Some(v), None) => err(s"1st value $v while 2nd value is None")
      case (Some(v1), Some(v2)) => f(v1, v2).map(p => (Some(p._1), Some(p._2)))
    }

}
