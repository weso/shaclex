package es.weso.shex.validator
import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PrefixMap
import es.weso.typing._
import es.weso.rdf.nodes._
import es.weso.shapeMaps.{BNodeLabel, IRILabel => IRIMapLabel, _}
import es.weso.shex.{ShapeLabel, ViolationError}
import io.circe.Json

case class ShapeTyping(t: Typing[RDFNode, ShapeType, ViolationError, String]) extends LazyLogging {

  def getOkValues(node: RDFNode): Set[ShapeType] =
    t.getOkValues(node)

  def getFailedValues(node: RDFNode): Set[ShapeType] =
    t.getFailedValues(node)

  // TODO Review these definitions in case of anonymous shapes...
  def hasInfoAbout(node: RDFNode, label: ShapeLabel): Boolean =
    hasType(node, label) || hasNoType(node, label)

  def hasType(node: RDFNode, label: ShapeLabel): Boolean = {
    !getOkValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def hasNoType(node: RDFNode, label: ShapeLabel): Boolean = {
    !getFailedValues(node).filter(_.hasLabel(label)).isEmpty
  }

  def getTypingResult(node: RDFNode, label: ShapeLabel): Option[TypingResult[ViolationError, String]] =
    t.getMap.get(node).map(_.toList.filter(_._1.label == Some(label)).map(_._2).head)

  def addType(node: RDFNode, shapeType: ShapeType): ShapeTyping =
    this.copy(t = t.addType(node, shapeType))

  def addEvidence(node: RDFNode, shapeType: ShapeType, evidence: String): ShapeTyping =
    this.copy(t = t.addEvidence(node, shapeType, evidence))

  def addNotEvidence(node: RDFNode, shapeType: ShapeType, err: ViolationError): ShapeTyping =
    this.copy(t = t.addNotEvidence(node, shapeType, err))

  def getMap: Map[RDFNode, Map[ShapeType, TypingResult[ViolationError, String]]] =
    t.getMap

  override def toString = showShapeTyping

  private def cnvShapeType(s: ShapeType): Either[String, ShapeMapLabel] = s.label match {
    case None => Left(s"Can't create Result shape map for a shape expression without label. ShapeExpr: ${s.shape}")
    case Some(lbl) => lbl.toRDFNode match {
      case i: IRI => Either.right(IRIMapLabel(i))
      case b: BNodeId => Either.right(BNodeLabel(b))
      case _ => Left(s"Can't create Result shape map for a shape expression with label: $lbl")
    }
  }

  private def cnvTypingResult(t: TypingResult[ViolationError, String]): Info = {
    val status = if (t.isOK) Conformant else NonConformant
    val reason =
      if (t.isOK) t.getEvidences.map(_.mkString("\n"))
      else t.getErrors.map(_.mkString("\n"))
    val appInfo = Json.fromString("Shaclex")
    Info(status, reason, appInfo)
  }

  private def typing2Labels(m: Map[ShapeType, TypingResult[ViolationError, String]]
                   ): Either[String, Map[ShapeMapLabel, Info]] = {
    def processType(m: Either[String, Map[ShapeMapLabel, Info]],
                    current: (ShapeType, TypingResult[ViolationError, String])
                   ): Either[String, Map[ShapeMapLabel, Info]] =
      cnvShapeType(current._1) match {
        case Left(s) => {
          logger.info(s)
          m
        }
        case Right(label) => {
          val info = cnvTypingResult(current._2)
          m.map(_.updated(label, info))
        }
      }
    val zero: Either[String, Map[ShapeMapLabel, Info]] = Either.right(Map[ShapeMapLabel, Info]())
    m.foldLeft(zero)(processType)
  }

  def toShapeMap(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): Either[String, ResultShapeMap] = {
    type Result = Either[String, ResultShapeMap]
    def combine(m: Result,
                current: (RDFNode, Map[ShapeType, TypingResult[ViolationError, String]])
               ): Result = for {
      rm <- m
      ls <- typing2Labels(current._2)
    } yield
      if (!ls.isEmpty) rm.addNodeAssociations(current._1, ls)
      else rm
    val zero: Either[String, ResultShapeMap] =
      Either.right(ResultShapeMap.empty.
        addNodesPrefixMap(nodesPrefixMap).
        addShapesPrefixMap(shapesPrefixMap)
      )
    getMap.foldLeft(zero)(combine)
  }

  def showShapeTyping: String = {
    import ShapeTyping._
    t.show
  }
}

object ShapeTyping {

  def emptyShapeTyping: ShapeTyping = {
    val emptyTyping: Typing[RDFNode, ShapeType, ViolationError, String] = Typing.empty
    ShapeTyping(emptyTyping)
  }

  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    def show(n: RDFNode) = s"$n"
  }

  implicit def showShapeTyping = new Show[ShapeTyping] {
    override def show(t: ShapeTyping): String = {
      t.showShapeTyping
    }
  }

  implicit def monoidShapeTyping = new Monoid[ShapeTyping] {
    override def empty: ShapeTyping = emptyShapeTyping

    override def combine(
      t1: ShapeTyping,
      t2: ShapeTyping): ShapeTyping =
      ShapeTyping(t1.t.combineTyping(t2.t))
  }

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping = {
    ShapeTyping(Typing.combineTypings(ts.map(_.t)))
  }

  implicit def showPair = new Show[(ShapeTyping, Evidences)] {
    def show(e: (ShapeTyping, Evidences)): String = {
      s"Typing: ${e._1.show}\n Evidences:\n${e._2.show}"
    }
  }

}
