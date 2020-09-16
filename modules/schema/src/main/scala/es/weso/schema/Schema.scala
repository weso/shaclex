package es.weso.schema
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shapeMaps.ShapeMap
import es.weso.utils.FileUtils
import cats.effect._

abstract class Schema {

  /**
   * Name of this schema. Example: ShEx, Shacl_TQ, ...
   */
  def name: String

  /**
   * Supported input formats
   */
  def formats: Seq[String]

  def validate(rdf: RDFReader, trigger: ValidationTrigger): IO[Result]

  def validate(
    rdf: RDFReader,
    triggerMode: String,
    shapeMap: String,
    optNode: Option[String],
    optShape: Option[String],
    nodePrefixMap: PrefixMap = PrefixMap.empty,
    shapesPrefixMap: PrefixMap = pm): IO[Result] = {
    val base = Some(FileUtils.currentFolderURL)
    ValidationTrigger.findTrigger(triggerMode, shapeMap, base, optNode, optShape, nodePrefixMap, shapesPrefixMap) match {
      case Left(err) => {
        IO(Result.errStr(s"Cannot get trigger: $err. TriggerMode: $triggerMode, prefixMap: $pm"))
      }
      case Right(trigger) =>
        validate(rdf, trigger)
    }
  }

  def fromString(cs: CharSequence, format: String, base: Option[String]): IO[Schema]

  def fromRDF(rdf: RDFReader): IO[Schema]

  def serialize(format: String, base: Option[IRI] = None): IO[String]

  def defaultFormat: String = formats.head

  def defaultTriggerMode: ValidationTrigger

  /**
   * Creates an empty schema
   */
  def empty: Schema

  /**
   * List of shapes
   */
  def shapes: List[String]

  /**
   * Prefix Map of this schema
   */
  def pm: PrefixMap

  def convert(targetFormat: Option[String],
              targetEngine: Option[String],
              base: Option[IRI]
             ): IO[String]

  def info: SchemaInfo

  def toClingo(rdf: RDFReader, shapeMap: ShapeMap): IO[String]

}
