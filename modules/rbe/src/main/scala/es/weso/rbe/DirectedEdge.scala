package es.weso.rbe

/**
 * Represents directed edges
 *
 * It can be [[DirectEdge direct]] or [[InverseEdge inverse]]
 */
trait DirectedEdge[Edge]

case class DirectEdge[Edge](edge: Edge) extends DirectedEdge[Edge]
case class InverseEdge[Edge](edge: Edge) extends DirectedEdge[Edge]