package es.weso.uml

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset

import es.weso.uml.UMLDiagram.UML
import net.sourceforge.plantuml.SourceStringReader
import net.sourceforge.plantuml.api.PlantumlUtils

object UMLDiagram {

  type NodeId = String

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

  case class ValueConstraint(name: String, href: Option[String]) extends UMLEntry
  case class UMLField(name: String, href: Option[String], valueConstraints: List[ValueConstraint], card: UMLCardinality) extends UMLEntry


  case class UMLClass(id: NodeId, label: String, href: Option[String], entries: List[List[UMLEntry]])

  case class UMLLink(source: UMLClass, target: UMLClass, label: String, href: String, card: UMLCardinality)

  /**
    * Represents an UML class diagram that can be serialized to PlantUML syntax
    *
    */
  case class UML(classes: Map[NodeId, UMLClass], links: List[UMLLink]) {

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
        sb.append(s"""${link.source.id} --> "${link.card}" ${link.target.id} : [[${link.href} ${link.label}]]\n""")
      }
      sb.append("@enduml\n")
      sb.toString
    }

    def toSVG: String = {
      val reader: SourceStringReader = new SourceStringReader(this.toPlantUML)
      val os: ByteArrayOutputStream = new ByteArrayOutputStream()
      import net.sourceforge.plantuml.FileFormat
      import net.sourceforge.plantuml.FileFormatOption
      val desc = reader.generateImage(os, new FileFormatOption(FileFormat.SVG))
      os.close
      val svg: String = new String(os.toByteArray(), Charset.forName("UTF-8"))
      svg
    }
  }

  object UML {
    def empty: UML = UML(Map(), List())
    def external = ValueConstraint("External",None)
    def iriKind = ValueConstraint("IRI",None)
    def bnodeKind = ValueConstraint("BNode",None)
    def nonLiteralKind = ValueConstraint("NonLiteral",None)
    def literalKind = ValueConstraint("Literal",None)

  }
}

