package es.weso.slang

import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
import es.weso.rdf.triples.RDFTriple
import cats._
import cats.data._
import cats.implicits._

// Naïve implementation of SLang validation

object Validation {

  type State = ShapesMap
  type SV[A] = StateT[Id,State,A]
  type Validation[A] = EitherT[SV,String,A]

  def ok[A](x: A): Validation[A] = EitherT.pure(x)
  def getShapesMap: Validation[ShapesMap] =
    EitherT.liftF(StateT.get[Id,ShapesMap])

  def updateShapesMap(fn: ShapesMap => ShapesMap): Validation[Unit] = {
    EitherT.liftF(StateT.modify(fn))
  }

  def fromEither[A](e: Either[String,A]): Validation[A] =
    EitherT.fromEither(e)

  def runValidation(node: RDFNode, shape: SLang, rdf: RDFReader, schema: SchemaS): Either[String,ShapesMap] = {
    val (sm,v) = validate(node, shape, rdf, schema).value.run(ShapesMap.empty)
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
        println(s"QualifiedArc($pp,$shape,$card)?")
        for {
          neighbourhood <- fromEither(rdf.triplesWithSubject(node))
          predicates = pp match {
            case Pred(p)   => Set(p)
            case NoPreds(ps) => neighbourhood.map(_.pred).diff(ps)
          }
          count <- countArcsWithShape(predicates, neighbourhood, shape, rdf, schema)
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