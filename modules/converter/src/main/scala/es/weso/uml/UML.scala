package es.weso.uml

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import es.weso.shex.ShapeLabel
import es.weso.uml.UMLDiagram.UML
import net.sourceforge.plantuml.SourceStringReader
import net.sourceforge.plantuml.api.PlantumlUtils

object UMLDiagram {

  type NodeId = Int
  type Name   = String
  type HRef   = String

  sealed abstract class UMLCardinality
  case object Star extends UMLCardinality {
    override def toString = "*"
  }
  case object Plus extends UMLCardinality {
    override def toString = "+"
  }
  case object Optional extends UMLCardinality {
    override def toString = "?"
  }
  case class Range(min: Int, max: IntOrUnbounded) extends UMLCardinality {
    override def toString = s"{$min,$max}"
  }
  sealed abstract class IntOrUnbounded
  case object Unbounded extends IntOrUnbounded {
    override def toString = "&#8734;"
  }
  case class IntMax(v: Int) extends IntOrUnbounded {
    override def toString: String = v.toString
  }
  case object NoCard extends UMLCardinality {
    override def toString = ""
  }

  sealed abstract class UMLEntry

  sealed abstract class ValueConstraint extends UMLEntry
  case class DatatypeConstraint(name: Name,
                                href: String
                               ) extends ValueConstraint

  case class Constant(name: Name) extends ValueConstraint

  case class ValueSet(values: List[Value]) extends ValueConstraint

  case class ValueExpr(operator: Name, vs: List[ValueConstraint]) extends ValueConstraint

  case class Value(name: String, href: Option[String])

  case class UMLField(name: Name,
                      href: Option[HRef],
                      valueConstraints: List[ValueConstraint],
                      card: UMLCardinality
                     ) extends UMLEntry

  case class FieldExpr(operator: Name,
                       es: List[UMLField]
                      ) extends UMLEntry

  case class UMLClass(id: NodeId, label: Name, href: Option[HRef], entries: List[List[UMLEntry]])

  case class UMLLink(source: NodeId, target: NodeId, label: Name, href: HRef, card: UMLCardinality)

  /**
    * Represents an UML class diagram that can be serialized to PlantUML syntax
    *
    */
  case class UML(labels: Map[ShapeLabel,NodeId], classes: Map[NodeId, UMLClass], links: List[UMLLink]) {

    /**
      * Adds a label to a UML diagram
      * If exists, return the existing nodeId
      * @param label
      * @return a pair with the updated UML diagram and the nodeId
      */
    def newLabel(label: ShapeLabel): (UML, NodeId) = {
      labels.get(label).fold{
        val id = this.labels.size
        (this.copy(labels = this.labels.updated(label, id)), id)
      } { id =>
        (this, id)
      }
    }

    def getId(label: ShapeLabel): Option[NodeId] = labels.get(label)

    def addClass(cls: UMLClass): UML = {
      this.copy(classes = classes.updated(cls.id,cls))
    }

    def addLink(link: UMLLink): UML =
      this.copy(links = link :: links)


    private def strHref(href: Option[HRef],
                        lbl: Name): String = href match {
      case None => ""
      case Some(href) => s"[[${href} ${lbl}]]"
    }

    def cnvValueConstraint(vc: ValueConstraint): String = vc match {
      case Constant(c) => c
      case ValueSet(vs) => {
        "[ " + vs.map{ v => v.href match {
          case None => v.name
          case Some(href) => s"[[${href} ${v.name}]]" }
        }.mkString(" ") + " ]"
      }
      case DatatypeConstraint(name,href) => {
        s"[[${href} ${name}]] "
      }
      case ValueExpr(op,vs) => vs.map(cnvValueConstraint(_)).mkString(s" ${op} ")
    }

    def cnvFieldExpr(fe: FieldExpr): String = {
      fe.es.map(cnvEntry(_)).mkString(fe.operator)
    }

    def cnvEntry(entry: UMLEntry): String = entry match {
      case field: UMLField => cnvField(field)
      case vc: ValueConstraint => cnvValueConstraint(vc)
      case fe: FieldExpr => cnvFieldExpr(fe)
    }

    def cnvField(field: UMLField): String =
      s"${strHref(field.href,field.name)} : ${field.valueConstraints.map(cnvValueConstraint(_)).mkString(" ")} ${field.card}"

    def toPlantUML: String = {
      val sb = new StringBuilder
      sb.append("@startuml\n")
      classes.values.foreach { cls =>
        sb.append(s"""class "${cls.label}" as ${cls.id} <<(S,#FF7700)>> ${strHref(cls.href,cls.label)} {\n""")
        cls.entries.foreach { entryLs =>
          entryLs.foreach { entry =>
            sb.append(cnvEntry(entry))
            sb.append("\n")
          }
          sb.append("--\n")
        }
        sb.append("}\n")
      }
      links.foreach { link =>
        sb.append(s"""${link.source} --> "${link.card}" ${link.target} : [[${link.href} ${link.label}]]\n""")
      }
      sb.append("@enduml\n")
      sb.toString
    }

    def toSVG: String = {
      import net.sourceforge.plantuml.FileFormat
      import net.sourceforge.plantuml.FileFormatOption
      val reader: SourceStringReader = new SourceStringReader(this.toPlantUML)
      val os: ByteArrayOutputStream = new ByteArrayOutputStream()
      val desc = reader.generateImage(os, new FileFormatOption(FileFormat.SVG))
      os.close
      val svg: String = new String(os.toByteArray(), Charset.forName("UTF-8"))
      svg
    }
  }

  object UML {
    def empty: UML = UML(Map(),Map(), List())
    lazy val external = Constant("External")
    lazy val iriKind = Constant("IRI")
    lazy val bnodeKind = Constant("BNode")
    lazy val nonLiteralKind = Constant("NonLiteral")
    lazy val literalKind = Constant("Literal")
    lazy val umlClosed = Constant("Closed")
    lazy val anyConstraint = Constant(".")
    def datatype(label: String, href: String) = DatatypeConstraint(label,href)

  }
}

