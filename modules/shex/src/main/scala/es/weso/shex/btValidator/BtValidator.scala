package es.weso.shex.btValidator

import cats.Id
import cats.data.{EitherT, Kleisli, ReaderT}
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps.{FixedShapeMap, ResultShapeMap, ShapeMapLabel}
import es.weso.shex.{Schema, Shape, TripleExpr, VarTable}
import es.weso.typing.Typing

/**
  * Backtracking validator
  */
case class BtValidator(schema:Schema)  {

  type ReaderRDF[A] = ReaderT[Id,RDFReader,A]
  type ReaderTyping[A] = ReaderT[ReaderRDF, Typing[RDFNode,ShapeMapLabel,ShExErr,List[String]], A]
  type Check[A] = EitherT[ReaderTyping,ShExErr, A]

  def getRDF: Check[RDFReader] = {
    val r1: ReaderRDF[RDFReader] = ReaderT.ask
    val r2: ReaderTyping[RDFReader] = ReaderT.liftF(r1)
    EitherT.liftF[ReaderTyping,ShExErr,RDFReader](r2)
  }

  def ok[A](x: A): Check[A] = EitherT.rightT[ReaderTyping,ShExErr](x)

  def checkNodeTripleExpr(node: RDFNode, te: TripleExpr): Check[(ResultShapeMap,VarTable)] = ???


  def checkShapeMap(shapeMap: FixedShapeMap, rdf: RDFReader): Check[ResultShapeMap] = {
    ???
  }

  def runCheck[A](rdf: RDFReader, c: Check[A]): Either[ShExErr,A] = {
    c.value.run(Typing.empty).run(rdf)
  }

}