package es.weso.shapeMaps

import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.utils.MapUtils._


case class ResultShapeMap(
  resultMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  def addShapesPrefixMap(pm: PrefixMap): ResultShapeMap =
    this.copy(shapesPrefixMap = pm)

  def addNodesPrefixMap(pm: PrefixMap): ResultShapeMap =
    this.copy(nodesPrefixMap = pm)

  def addNodeAssociations(node: RDFNode, mapLabels: Map[ShapeMapLabel, Info]): ResultShapeMap = {
    resultMap.get(node) match {
      case None => this.copy(resultMap = this.resultMap.updated(node, mapLabels))
      case Some(vs) => this.copy(resultMap = this.resultMap.updated(node, vs ++ mapLabels))
    }
  }

  def getConformantShapes(node: RDFNode): List[ShapeMapLabel] = {
    resultMap.get(node) match {
      case None => List()
      case Some(m) => m.toList.collect { case p if p._2.status == Conformant => p._1 }
    }
  }

  def getInfo(node: RDFNode, shape: ShapeMapLabel): Info =
    resultMap.get(node) match {
      case None => {
        Info.undefined(s"Node ${node.show} not found in ${Show[ShapeMap].show(this)}")
      }
      case Some(m) => m.get(shape) match {
        case None => Info.undefined(s"Node ${node.show} has no value for shape ${shape.show} in ${Show[ShapeMap].show(this)}")
        case Some(info) => info
      }
  }

  def getNonConformantShapes(node: RDFNode): List[ShapeMapLabel] = {
    resultMap.get(node) match {
      case None => List()
      case Some(m) => m.toList.collect { case p if p._2.status == NonConformant => p._1 }
    }
  }

  def hasShapes(node: RDFNode): Seq[ShapeMapLabel] = {
    resultMap.get(node).map(_.keySet.toSeq).getOrElse(Seq())
  }

  def containsDeclaration(node: RDFNode): Boolean =
    resultMap.keySet.contains(node)

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
    val nodes1 = resultMap.keySet.filter(_.isIRI)
    val nodes2 = other.resultMap.keySet.filter(_.isIRI)
    val delta = (nodes1 diff nodes2) union (nodes2 diff nodes1)
    if (!delta.isEmpty) {
      Left(s"""|Nodes in map1 != nodes in map2. Delta: ${delta.map(nodesPrefixMap.qualify(_)).mkString(",")}
               |Nodes1=${nodes1.map(nodesPrefixMap.qualify(_)).mkString(",")}
               |Nodes2=${nodes2.map(nodesPrefixMap.qualify(_)).mkString(",")}
               |Map1=$this
               |Map2=$other""".stripMargin)
    } else {
      val es = resultMap.filter(!_._1.isBNode).map {
        case (node, shapes1) => other.resultMap.get(node) match {
          case None => Left(s"Node ${nodesPrefixMap.qualify(node)} appears in map1 with shapes ${shapes1} but not in map2")
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
    if (shapes1.keySet.filter(! _.isBNodeLabel).size != shapes2.keySet.filter(! _.isBNodeLabel).size)
      Left(s"Node $node has different values. Map1: $shapes1, Map2: $shapes2")
    else {
      val es: List[Either[String, Boolean]] = shapes1.filter(!_._1.isBNodeLabel).map {
        case (label, info1) => shapes2.get(label) match {
          case None => Left(s"Node $node has label $label in map1 but doesn't have that label in map2. Shapes1 = $shapes1, Shapes2 = $shapes2")
          case Some(info2) =>
            if (info1.status == info2.status) Right(true)
            else Left(
              s"""|Node $node is ${info1.status} for label $label in map1 and ${info2.status} in map2
                 |Reason1: ${info1.reason}
                 |Reason2: ${info2.reason}
               """.stripMargin)
        }
      }.toList
      val r: Either[String, List[Boolean]] = seqEither(es) // es.sequence
      r.map(_ => true)
    }
  }

  private type ES[A] = Either[String, A]

  private def seqEither[A,B](es: List[Either[String,B]]): Either[String,List[B]] = es.sequence[ES,B]

  private def cnvResultMap(cnvNode: RDFNode => RDFNode,
                           cnvLabel: ShapeMapLabel => ShapeMapLabel
                          ): ResultShapeMap = {
    ResultShapeMap(
      cnvMapMap(resultMap, cnvNode, cnvLabel, identity[Info]),
      nodesPrefixMap,
      shapesPrefixMap
    )
  }

  override def relativize(maybeBase: Option[IRI]): ShapeMap = maybeBase match {
    case None => this
    case Some(base) => cnvResultMap(_.relativize(base), _.relativize(base))
  }

}

object ResultShapeMap {
  def empty = ResultShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)

}

