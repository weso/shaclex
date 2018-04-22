package es.weso.uml

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import es.weso.shex.ShapeLabel
import es.weso.uml.UMLDiagram.UML
import net.sourceforge.plantuml.SourceStringReader
import net.sourceforge.plantuml.api.PlantumlUtils

object UMLDiagram {

  type NodeId = Int

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

  case class ValueConstraint(name: String,
                             href: Option[String]
                            ) extends UMLEntry

  case class UMLField(name: String,
                      href: Option[String],
                      valueConstraints: List[ValueConstraint],
                      card: UMLCardinality
                     ) extends UMLEntry

  case class UMLClass(id: NodeId, label: String, href: Option[String], entries: List[List[UMLEntry]])

  case class UMLLink(source: NodeId, target: NodeId, label: String, href: String, card: UMLCardinality)

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


    private def strHref(href: Option[String],
                        lbl: String) = href match {
      case None => ""
      case Some(href) => s"[[${href} ${lbl}]]"
    }


    def toPlantUML: String = {
      val sb = new StringBuilder
      sb.append("@startuml\n")
      classes.values.foreach { cls =>
        sb.append(s"""class "${cls.label}" as ${cls.id} <<(S,#FF7700)>> ${strHref(cls.href,cls.label)} {\n""")
        cls.entries.foreach { entryLs =>
          entryLs.foreach { entry => entry match {
            case field: UMLField => {
              sb.append(s"""${strHref(field.href,field.name)}""")
              sb.append(" : ")
              field.valueConstraints.foreach { valueConstraint =>
                sb.append(valueConstraint.href match {
                  case None => valueConstraint.name
                  case Some(href) => s"[[${href} ${valueConstraint.name}]] "
                })
              }
              sb.append(field.card)
            }
            case vc: ValueConstraint => {
              sb.append(vc.href match {
                case None => vc.name
                case Some(href) => s"[[${vc.href} ${vc.name}]] "
              })
            }
          }


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
    lazy val external = ValueConstraint("External",None)
    lazy val iriKind = ValueConstraint("IRI",None)
    lazy val bnodeKind = ValueConstraint("BNode",None)
    lazy val nonLiteralKind = ValueConstraint("NonLiteral",None)
    lazy val literalKind = ValueConstraint("Literal",None)
    lazy val umlClosed = ValueConstraint("Closed",None)
    lazy val anyConstraint = ValueConstraint(".",None)
    def datatype(label: String, href: String) = ValueConstraint(label,Some(href))

  }
}

