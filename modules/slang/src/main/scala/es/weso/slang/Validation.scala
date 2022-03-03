package es.weso.slang

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
import es.weso.rdf.triples.RDFTriple
//import cats._
import cats.data._
import cats.implicits._
import cats.effect.IO
import fs2.Stream

// Naïve implementation of SLang validation

object Validation extends LazyLogging{

  type State = ShapesMap
  type SV[A] = StateT[IO,State,A]
  type Validation[A] = EitherT[SV,String,A]

  def ok[A](x: A): Validation[A] = EitherT.pure(x)
  def getShapesMap: Validation[ShapesMap] =
    EitherT.liftF(StateT.get[IO,ShapesMap])

  def updateShapesMap(fn: ShapesMap => ShapesMap): Validation[Unit] = {
    EitherT.liftF(StateT.modify(fn))
  }

  def fromEither[A](e: Either[String,A]): Validation[A] =
    EitherT.fromEither(e)

  def fromIO[A](io: IO[A]): Validation[A] =
    EitherT.liftF(StateT.liftF(io))

  def fromStreamIO[A](io: Stream[IO,A]): Validation[List[A]] =
    fromIO(io.compile.toList)


  def runValidation(node: RDFNode, shape: SLang, rdf: RDFReader, schema: SchemaS): IO[Either[String,ShapesMap]] = for {
    pair <- validate(node, shape, rdf, schema).value.run(ShapesMap.empty)
  } yield {
    val (sm,v) = pair
    v.fold(Left(_), _ => Right(sm))
  }


  def validate(node: RDFNode, shape: SLang, rdf: RDFReader, schema: SchemaS): Validation[Val] = {
    shape match {
      case STrue => cond(true,node,shape)
      case And(s1, s2) => for {
        v1 <- validate(node, s1, rdf, schema)
        v2 <- validate(node, s2, rdf, schema)
        r <- cond(v1.isConforming && v2.isConforming, node,shape)
      } yield r
      case BNodeKind =>
        cond(node.isBNode, node, shape)
     case IRIKind =>
        cond(node.isIRI,node, shape)
      case Datatype(iri) =>
        cond(hasDatatype(node,iri), node, shape)

      case Not(s) => for {
        v <- validate(node, s, rdf, schema)
        r <- cond(!v.isConforming, node, shape)
      } yield r

      case Ref(lbl) => {
        schema.getLabel(lbl) match {
          case Some(s) => for {
            v <- validate(node, s, rdf, schema)
            r <- cond(v.isConforming,node,shape)
          } yield r
          case None        => throw new Exception(s"Label $lbl not found in Schema")
        }
      }
      case QualifiedArc(pp, shape, card) => {
        logger.debug(s"QualifiedArc($pp,$shape,$card)?")
        for {
          neighbourhood <- fromStreamIO(rdf.triplesWithSubject(node))
          predicates = pp match {
            case Pred(p)   => Set(p)
            case NoPreds(ps) => neighbourhood.map(_.pred).toSet.diff(ps)
          }
          count <- countArcsWithShape(predicates, neighbourhood.toSet, shape, rdf, schema)
          r <- cond(card.satisfies(count), node, shape)
        } yield r
      }
    }
  }

  private def cond(cond: Boolean, node: RDFNode, shape: SLang): Validation[Val] = if (cond) for {
    _ <- updateShapesMap(_.conform(node,shape))
  } yield Conforms
  else for {
    _ <- updateShapesMap(_.notConform(node,shape))
  } yield NotConforms

  private def hasDatatype(node: RDFNode, dt: IRI): Boolean = node match {
    case l: RDFLiteral => l.dataType == dt
    case _ => false
  }
  private def countArcsWithShape(predicates: Set[IRI],
                                 triples: Set[RDFTriple],
                                 shape: SLang,
                                 rdf: RDFReader,
                                 schema: SchemaS
                                ): Validation[Int] = {
    val values = triples.filter(t => predicates.contains(t.pred)).map(_.obj).toList
    values.map(v => validate(v, shape, rdf, schema)).sequence[Validation,Val].map(countConforming)
  }

  private def countConforming(ls: List[Val]): Int = ls.filter(_.isConforming).length

}