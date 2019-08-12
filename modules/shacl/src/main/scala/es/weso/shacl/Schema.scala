package es.weso.shacl

import es.weso.rdf.{PrefixMap, RDFBuilder}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shacl.converter.Shacl2RDF

import scala.util.{Either, Left, Right}
import sext._

case class Schema(pm: PrefixMap,
                  imports: List[IRI],
                  entailments: List[IRI],
                  shapesMap: Map[RefNode, Shape],
                  propertyGroups: Map[RefNode, PropertyGroup]
                 ) {

  lazy val shapes: Seq[Shape] =
    shapesMap.toSeq.map(_._2)

  lazy val shapeRefs: Seq[RefNode] =
    shapesMap.keys.toSeq

  /**
    * Get the shape associated to an IRI
    * @param node IRI that identifies a shape
    */
  def shape(node: RDFNode): Either[String, Shape] =
    shapesMap.get(RefNode(node)) match {
      case None => Left(s"Not found $node in Schema")
      case Some(shape) => Right(shape)
    }

  private[shacl] def siblingQualifiedShapes(s: RefNode): List[RefNode] = {
    val parentShapes: List[Shape] =
      parents(s).
        map(shapesMap.get(_)).
        collect { case Some(shape) => shape }
    val qualifiedPropertyShapes =
      parentShapes.
        flatMap(_.propertyShapes).
        filter(_ != s)
    collectQualifiedValueShapes(qualifiedPropertyShapes)
  }

  private def collectQualifiedValueShapes(ls: List[RefNode]): List[RefNode] = {
    val zero: List[RefNode] = List()
    def comb(xs: List[RefNode], x: RefNode): List[RefNode] =
      qualifiedShapes(x) ++ xs
    ls.foldLeft(zero)(comb)
  }

  private def qualifiedShapes(p: RefNode): List[RefNode] = shapesMap.get(p) match {
    case None => List()
    case Some(shape) =>
      shape.components.collect { case x: QualifiedValueShape => x.shape }.toList
  }

  /* Find shape x such that x sh:property p
   */
  private[shacl] def parents(p: RefNode): List[RefNode] = {
    shapesWithPropertyShape(this.shapeRefs, p)
  }

  private def shapesWithPropertyShape(ls: Seq[RefNode], p: RefNode): List[RefNode] = {
    ls.filter(hasPropertyShape(_, p)).toList
  }

  private def hasPropertyShape(s: RefNode, p: RefNode): Boolean = {
    shapesMap.get(s) match {
      case None => false // TODO: Maybe raise an error
      case Some(shape) =>
        if (shape.propertyShapes.contains(p)) true
        else false
    }
  }

  /**
    * Get the sequence of sh:targetNode declarations
    */
  def targetNodeShapes: Seq[(RDFNode, Shape)] = {
    val zero: Seq[(RDFNode, Shape)] = Seq()
    def comb(rs: Seq[(RDFNode, Shape)], s: Shape): Seq[(RDFNode, Shape)] = {
      val ns: Seq[RDFNode] = s.targetNodes
      ns.map(n => (n, s)) ++ rs
    }
    shapes.foldLeft(zero)(comb)
  }

  /**
    * Get the sequence of `sh:targetNode` declarations
    * @return a list of pairs (n,s) where n is the IRI of a node
    * and s is the IRI of a shape
    */
  def targetNodeDeclarations: Seq[(RDFNode, RDFNode)] = {
    targetNodeShapes.map {case (node, shape) => (node, shape.id) }
  }

  def serialize(format: String = "TURTLE",
                base: Option[IRI],
                builder: RDFBuilder): Either[String, String] = {
    format.toUpperCase match {
      case "TREE" => {
        Right(s"PrefixMap ${pm.treeString}\nShapes: ${shapes.treeString}")
      }
      case _ => {
        new Shacl2RDF {}.serialize(this, format, base, builder.empty)
      }
    }
  }

}

// Companion iriObjects
object Schema {
  val empty =
    Schema(
      pm = SHACLPrefixes.defaultPrefixMap,
      imports = List(),
      entailments = List(),
      shapesMap = Map(),
      propertyGroups = Map()
    )
}
