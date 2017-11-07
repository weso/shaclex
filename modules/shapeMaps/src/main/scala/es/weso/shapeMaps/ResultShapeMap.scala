package es.weso.shapeMaps

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode
import cats._
import data._
import cats.implicits._
import io.circe._
import io.circe.syntax._

case class ResultShapeMap(
  resultMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  def addNodeAssociations(node: RDFNode, mapLabels: Map[ShapeMapLabel, Info]): ResultShapeMap = {
    resultMap.get(node) match {
      case None => this.copy(resultMap = this.resultMap.updated(node, mapLabels))
      case Some(vs) => this.copy(resultMap = this.resultMap.updated(node, vs ++ mapLabels))
    }
  }

  def hasShapes(node: RDFNode): Seq[ShapeMapLabel] = {
    resultMap.get(node).map(_.keySet.toSeq).getOrElse(Seq())
  }

  def noSolutions = resultMap.isEmpty

  val associations: List[Association] = resultMap.toList.flatMap {
    case (node, labelsMap) => {
      labelsMap.toList.map {
        case (shapeLabel, info) => {
          Association(RDFNodeSelector(node), shapeLabel, info)
        }
      }
    }
  }

  override def addAssociation(a: Association): Either[String, ResultShapeMap] = {
    a.node match {
      case RDFNodeSelector(node) => {
        resultMap.get(node) match {
          case None => Right(this.copy(resultMap = resultMap.updated(node, Map(a.shape -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shape) match {
              case None => Right(this.copy(resultMap = resultMap.updated(node, labelsMap.updated(a.shape, a.info))))
              case Some(info) =>
                if (info.status == a.info.status) Right(this)
                else Left(s"Cannot add association with contradictory status: Association: ${a}, Labels map: ${labelsMap}")
            }
          }
        }
      }
      case _ => Left(s"Only RDFNode's can be added as associations to fixedShapeMaps. Value = ${a.node}")
    }
  }

  def compareWith(other: ResultShapeMap): Either[String, Boolean] = {
    val nodes1 = resultMap.keySet
    val nodes2 = other.resultMap.keySet
    val delta = (nodes1 diff nodes2) union (nodes2 diff nodes1)
    if (!delta.isEmpty) {
      Left(s"Nodes in map1 != nodes in map2. Delta: $delta\nNodes1 = ${nodes1}\nNodes2=${nodes2}\nMap1 = $resultMap\nMap2=$other")
    } else {
      val es = resultMap.map {
        case (node, shapes1) => other.resultMap.get(node) match {
          case None => Left(s"Node $node appears in map1 with shapes $shapes1 but not in map2")
          case Some(shapes2) => compareShapes(node, shapes1, shapes2)
        }
      }.toList
      seqEither(es).map(_ => true)
    }
  }

  private def compareShapes(
    node: RDFNode,
    shapes1: Map[ShapeMapLabel, Info],
    shapes2: Map[ShapeMapLabel, Info]): Either[String, Boolean] = {
    if (shapes1.keySet.size != shapes2.keySet.size)
      Left(s"Node $node has different values. Map1: $shapes1, Map2: $shapes2")
    else {
      val es: List[Either[String, Boolean]] = shapes1.map {
        case (label, info1) => shapes2.get(label) match {
          case None => Left(s"Node $node has label $label in map1 but doesn't have that label in map2. Shapes1 = $shapes1, Shapes2 = $shapes2")
          case Some(info2) =>
            if (info1.status == info2.status) Right(true)
            else Left(s"Status of node $node for label $label is ${info1.status} in map1 and ${info2.status} in map2")
        }
      }.toList
      val r: Either[String, List[Boolean]] = seqEither(es) // es.sequence
      r.map(_ => true)
    }
  }

  // The following code requires partial unification plugin
  // https://github.com/fiadliel/sbt-partial-unification
  def seqEither[A,B](es: List[Either[A,B]]): Either[A,List[B]] = es.sequence
  /*{
    def combine(rest: Either[A,List[B]],
                current: Either[A,B]): Either[A,List[B]] =
      current.fold(
        a => Left(a),
        b => rest.fold(
          a => Left(a),
          bs => Right(b :: bs)
        )
      )
    val zero: Either[A,List[B]] = Right(List())
    es.foldLeft(zero)(combine)
  } */
}

object ResultShapeMap {
  def empty = ResultShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)

}

