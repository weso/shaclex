package es.weso.shex.btValidator

import cats.Id
import cats.data.{EitherT, Kleisli, ReaderT}
import cats.kernel.Monoid
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps.{FixedShapeMap, ResultShapeMap, ShapeMapLabel}
import es.weso.shex._
import es.weso.typing.Typing

/**
  * Backtracking validator
  */
object BtValidator {

  type ShapeTyping = Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]]
  type ReaderRDF[A] = ReaderT[Id,RDFReader,A]
  type ReaderSchema[A] = ReaderT[ReaderRDF, Schema, A]
  type ReaderVarTable[A] = ReaderT[ReaderSchema,VarTable,A]
  type ReaderTyping[A] = ReaderT[ReaderVarTable, ShapeTyping, A]
  type Check[A] = EitherT[ReaderTyping,ShExErr, A]

  def getRDF: Check[RDFReader] = {
    val r1: ReaderRDF[RDFReader] = ReaderT.ask
    val r2: ReaderSchema[RDFReader] = ReaderT.liftF(r1)
    var r3: ReaderVarTable[RDFReader] = ReaderT.liftF(r2)
    val r4: ReaderTyping[RDFReader] = ReaderT.liftF(r3)
    EitherT.liftF[ReaderTyping,ShExErr,RDFReader](r4)
  }

  def getVarTable: Check[VarTable] = {
    ???
  }

  def ok[A](x: A): Check[A] = EitherT.rightT[ReaderTyping,ShExErr](x)
  def err[A](e: ShExErr): Check[A] = EitherT.leftT[ReaderTyping,A](e)
  def unimplemented[A](msg: String): Check[A] = err(Unimplemented(msg))

  def checkNodeTripleExpr(node: RDFNode, te: TripleExpr): Check[(ShapeTyping,VarTable)] =
    te match {
      case tc: TripleConstraint => checkNodeTripleConstraint(node,tc)
      case _ => unimplemented(s"checkNodeTripleExpr: $te")
    }

  def checkNodeTripleConstraint(node: RDFNode, tc: TripleConstraint): Check[(ShapeTyping, VarTable)] = ???

  def checkShapeMap(shapeMap: FixedShapeMap, rdf: RDFReader): Check[ResultShapeMap] = {
    ???
  }

  def runCheck[A](rdf: RDFReader, schema: Schema, c: Check[A]): Either[ShExErr,A] = {
    c.value.run(Typing.empty).run(Monoid[VarTable].empty).run(schema).run(rdf)
  }

}