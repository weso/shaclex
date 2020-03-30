package es.weso.slang

//import cats._
import cats.data._
import cats.effect.IO
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{Literal => RDFLiteral, _}
import es.weso.rdf.triples.RDFTriple
import fs2.Stream


// Validation with Answer-sets

object ValidateND {

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

  def fromStream[A](e: Stream[IO,A]): Validation[List[A]] =
    EitherT.liftF(StateT.liftF(e.compile.toList))


  case class Pair(node: RDFNode, shape: SLang)

  def runValidation(node: RDFNode, shape: SLang, rdf: RDFReader, schema: SchemaS): IO[Either[String,ShapesMap]] = for {
    pair <- validate(List(Pair(node, shape)), rdf, schema).value.run(ShapesMap.empty)
  } yield {
    val (sm,v) = pair
    v.fold(e => Left(e), _ => Right(sm))
  }

  def validate(pending: List[Pair], rdf: RDFReader, schema: SchemaS): Validation[Unit] = {
    println(s"Validate(${pending.map(_.toString).mkString(",")})")
    pending match {
    case Nil => ok(())
    case pair :: rest => {
      val (node, shape) = (pair.node, pair.shape)
      for {
          _ <- updateShapesMap(_.unknown(node, shape))
          sm <- getShapesMap
          _ <- { println(s"Validating |$node/$shape|\n---\n${sm}\n---"); ok(()) }
          _ <- if (sm.validated(node, shape)) validate(rest, rdf, schema)
          else
            shape match {
              case STrue =>
                for {
                  _ <- updateShapesMap(_.conform(node, shape))
                  _ <- validate(rest, rdf, schema)
                } yield (())
              case And(s1, s2) =>
                for {
                  _  <- validate(Pair(node, s1) :: Pair(node, s2) :: rest, rdf, schema)
                  _ <- {
                    val v = Val.and(sm.isConforming(node, s1), sm.isConforming(node, s2))
                    updateShapesMap(_.addVal(node, shape, v))
                  }
                  _ <- validate(rest, rdf, schema)
                } yield (())
              case BNodeKind => checkValidate(node.isBNode, node, shape, rest,rdf,schema)
              case IRIKind => checkValidate(node.isIRI, node, shape, rest,rdf,schema)
              case Datatype(iri) => checkValidate(hasDatatype(node, iri), node, shape, rest,rdf,schema)
              case Not(s) =>
                for {
                  _  <- validate(Pair(node, s) :: rest, rdf, schema)
                  newSM <- getShapesMap
                  _ <- newSM.isConforming(node, s) match {
                    case Conforms    => updateShapesMap(_.notConform(node, shape))
                    case NotConforms => updateShapesMap(_.conform(node, shape))
                    case Unknown =>
                      for {
                        _ <- updateShapesMap(_.notConform(node, s))
                        _ <- updateShapesMap(_.conform(node, shape))
                      } yield (())
                    case Inconsistent => updateShapesMap(_.addVal(node, shape, Inconsistent))
                  }
                 _ <- validate(rest,rdf,schema)
                } yield (())
              case Ref(lbl) => {
                schema.getLabel(lbl) match {
                  case Some(s) =>
                    for {
                      _  <- validate(Pair(node, s) :: rest, rdf, schema)
                      sm <- getShapesMap
                      _  <- cond(sm.isOk(node, s), node, shape)
                      _ <- validate(rest,rdf,schema)
                    } yield (())
                  case None => throw new Exception(s"Label $lbl not found in Schema")
                }
              }
              case QualifiedArc(pp, s, card) => {
                println(s"QualifiedArc($pp,$s,$card)?")
                for {
                  neighbourhood <- fromStream(rdf.triplesWithSubject(node))
                  predicates = pp match {
                    case Pred(p)   => Set(p)
                    case NoPreds(ps) => neighbourhood.map(_.pred).toSet.diff(ps)
                  }
                  count <- countArcsWithShape(predicates, rest, neighbourhood.toSet, s, rdf, schema)
                  _ <- { println(s"After countArcs: count= $count, card=$card, ${card.satisfies(count)}"); ok(()) }
                  _     <- cond(card.satisfies(count), node, shape)
                  sm1 <- getShapesMap
                  _ <- { println(s"After cond. shapeMap:\n---\n${sm1}\n---"); ok(()) }
//                  _ <- validate(rest,rdf,schema)
                } yield (())
              }
            }
      } yield (())
    }
    }
  }

  private def checkValidate(b: Boolean,
                            node: RDFNode,
                            shape: SLang,
                            rest: List[Pair],
                            rdf: RDFReader,
                            schema: SchemaS
                           ): Validation[Unit] = for {
    _ <- cond(b, node, shape)
    _ <- validate(rest, rdf, schema)
  } yield (())

  private def cond(cond: Boolean, node: RDFNode, shape: SLang): Validation[Unit] =
   if (cond) for {
    _ <- updateShapesMap(_.conform(node,shape))
   } yield ()
   else for {
    _ <- updateShapesMap(_.notConform(node,shape))
   } yield ()

  private def hasDatatype(node: RDFNode, dt: IRI): Boolean = node match {
    case l: RDFLiteral => l.dataType == dt
    case _ => false
  }

  private def countArcsWithShape(predicates: Set[IRI],
                                 rest: List[Pair],
                                 triples: Set[RDFTriple],
                                 shape: SLang,
                                 rdf: RDFReader,
                                 schema: SchemaS
                                ): Validation[Int] = {
    println(s"CountArcsWithShape(ps=$predicates, shape=$shape,rest=$rest, triples=${triples.map(_.toString).mkString(",")}")
    val values = triples.filter(t => predicates.contains(t.pred)).map(_.obj).toList
    val pairs = values.map(Pair(_,shape))
    for {
     _ <- validate(pairs ++ rest, rdf,schema)
     sm <- getShapesMap
     n = countOk(sm, values, shape)
     _ <- { println(s"Return of CountArcsWithShape, count=$n. New shapeMap:\n---${sm}\n---"); ok(()) }
    } yield n
  }

  private def countOk(sm: ShapesMap, values: List[RDFNode], shape: SLang): Int =
    values.filter(sm.isOk(_,shape)).size


}