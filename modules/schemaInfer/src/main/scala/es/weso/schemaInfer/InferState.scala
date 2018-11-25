package es.weso.schemaInfer
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES.xsd
import es.weso.shapeMaps._

case class InferState(schema: InferredSchema,
                      prefixMap: PrefixMap,
                      inferredShapeMap: ResultShapeMap,
                      visited: Set[RDFNode]
                     ) {

  def updateSchema(fn: InferredSchema => InferredSchema): InferState =
    this.copy(schema = fn(schema))

  def addPrefixMap(pm: PrefixMap): InferState =
    this.copy(prefixMap = this.prefixMap.merge(pm))

  def addPrefixDecl(p: Prefix, iri: IRI): InferState =
    this.copy(prefixMap = prefixMap.addPrefix(p,iri))

  def addNodeLabel(node: RDFNode, label: IRI): Either[String,InferState] = {
    val a = Association(RDFNodeSelector(node), IRILabel(label))
    inferredShapeMap.addAssociation(a).map(sm => this.copy(inferredShapeMap = sm))
  }

  def updateShapeMap(fn: ResultShapeMap => ResultShapeMap): InferState =
    this.copy(inferredShapeMap = fn(inferredShapeMap))

  def addVisited(node: RDFNode): InferState = this.copy(visited = visited + node)

  def isVisited(node: RDFNode): Boolean = visited.contains(node)

}

object InferState {
  def initial: InferState = {
    val prefixMap = PrefixMap.empty.addPrefix(Prefix("sx"), SxNamespace.sx).addPrefix(Prefix("xsd"), xsd)
    InferState(
      InferredSchema.empty,
      prefixMap,
      ResultShapeMap.empty,
      Set()
    )
  }
}
