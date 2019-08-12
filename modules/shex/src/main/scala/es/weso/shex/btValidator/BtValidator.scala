package es.weso.shex.btValidator

import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps._
import es.weso.shex._
import es.weso.typing.Typing

/**
  * Backtracking validator
  */
object BtValidator {

  type ShapeTyping = Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]]

  case class Env(rdf: RDFReader, schema: Schema, typing: ShapeTyping, table: VarTable)

  type ReaderEnv[A] = ReaderT[Id,Env,A]
  type Check[A] = EitherT[ReaderEnv,ShExErr, A]

  def getRDF: Check[RDFReader] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    EitherT.liftF[ReaderEnv,ShExErr,RDFReader](r.map(_.rdf))
  }

  def getSchema: Check[Schema] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    EitherT.liftF[ReaderEnv,ShExErr,Schema](r.map(_.schema))
  }

  def getTyping: Check[ShapeTyping] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    EitherT.liftF[ReaderEnv,ShExErr,ShapeTyping](r.map(_.typing))
  }

  def getVarTable: Check[VarTable] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    EitherT.liftF[ReaderEnv,ShExErr,VarTable](r.map(_.table))
  }

  def local[A](f: Env => Env)(c: Check[A]): Check[A] = {
    EitherT(c.value.local(f))
  }

  def localWithTable[A](f: VarTable => VarTable, c: Check[A]): Check[A] = {
    def fn: Env => Env = e => e.copy(table = f(e.table))
    local(fn)(c)
  }

  def localWithTyping[A](f: ShapeTyping => ShapeTyping, c: Check[A]): Check[A] = {
    def fn: Env => Env = e => e.copy(typing = f(e.typing))
    local(fn)(c)
  }

  def ok[A](x: A): Check[A] = x.pure[Check]
  def err[A](e: ShExErr): Check[A] = EitherT.leftT[ReaderEnv,A](e)
  def unimplemented[A](msg: String): Check[A] = err(Unimplemented(msg))
  def fromEither[A](e: Either[String,A]): Check[A] =
    EitherT.fromEither(e.leftMap(StringErr(_)))

  def checkNodeTripleExpr(node: RDFNode, te: TripleExpr): Check[(ShapeTyping,VarTable)] =
    te match {
      case tc: TripleConstraint => checkNodeTripleConstraint(node,tc)
      case _ => unimplemented(s"checkNodeTripleExpr: $te")
    }

  def checkNodeTripleConstraint(node: RDFNode,
                                tc: TripleConstraint
                               ): Check[(ShapeTyping, VarTable)] =
    if (tc.direct) {
      for {
        rdf <- getRDF
        triples <- fromEither(rdf.triplesWithSubjectPredicate(node, tc.predicate))
        typing <- getTyping
        table <- getVarTable
      } yield {
        val newTable = tc.optVariableDecl match {
          case None => table
          case Some(name) => table.set(name,triples.map(_.obj).toList)
        }
        (typing, newTable)
      }
    } else unimplemented("checkTripleConstraint with inverse ")


  def runCheck[A](rdf: RDFReader, schema: Schema, c: Check[A]): Either[ShExErr,A] = {
    val env = Env(rdf,schema,Typing.empty,Monoid[VarTable].empty)
    c.value.run(env)
  }

}