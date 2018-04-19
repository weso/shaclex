package es.weso.uml

import es.weso.uml.UMLDiagram.UML

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

  case class ValueConstraint(name: String, href: Option[String])

  case class UMLField(name: String, href: String, valueConstraints: List[ValueConstraint], card: UMLCardinality)

  case class UMLClass(id: NodeId, label: String, href: String, fields: List[UMLField])

  case class UMLLink(source: UMLClass, target: UMLClass, label: String, href: String, card: UMLCardinality)

  /**
    * Represents an UML class diagram that can be serialized to PlantUML syntax
    *
    */
  case class UML(classes: Map[NodeId, UMLClass], links: List[UMLLink]) {

    def addClass(cls: UMLClass): UML = {
      val id = "C" + nextId
      this.copy(classes = classes.updated(id,cls))
    }

    def addLink(link: UMLLink): UML =
      this.copy(links = link :: links)

    private def nextId = classes.size

    def toPlantUML: String = {
      val sb = new StringBuilder
      sb.append("@startuml\n")
      classes.values.foreach { cls =>
        sb.append(s"""class "${cls.label}" as ${cls.id} <<(S,#FF7700)>> [[${cls.href} ${cls.label}]]>> {\n""")
        cls.fields.foreach { field =>
          sb.append(s""" [[${field.href} ${field.name}]] """)
          field.valueConstraints.foreach { valueConstraint =>
            sb.append(valueConstraint.href match {
              case None => valueConstraint.name
              case Some(href) => s"[[${valueConstraint.href} ${valueConstraint.name}]] "
            })
          }
          sb.append(field.card)
          sb.append("\n")
        }
        sb.append("}\n")
      }
      links.foreach { link =>
        sb.append(s"""${link.source.id} --> "${link.card}" ${link.target.id} : [[${link.href} ${link.label}]]\n""")
      }
      sb.append("@enduml\n")
      sb.toString
    }
  }

  object UML {
    def empty: UML = UML(Map(), List())
  }
}

