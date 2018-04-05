package es.weso.server

import org.http4s.dsl.io.{OptionalQueryParamDecoderMatcher, QueryParamDecoderMatcher}

object QueryParams {
  object DataParameter extends QueryParamDecoderMatcher[String]("data")
  object OptDataParam extends OptionalQueryParamDecoderMatcher[String]("data")
  object OptEndpointParam extends OptionalQueryParamDecoderMatcher[String]("endpoint")
  object OptDataURLParam extends OptionalQueryParamDecoderMatcher[String]("dataURL")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object TargetDataFormatParam extends OptionalQueryParamDecoderMatcher[String]("targetDataFormat")
  object OptSchemaParam extends OptionalQueryParamDecoderMatcher[String]("schema")
  object SchemaURLParam extends OptionalQueryParamDecoderMatcher[String]("schemaURL")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")
  object TargetSchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("targetSchemaFormat")
  object TargetSchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("targetSchemaEngine")
  object OptTriggerModeParam extends OptionalQueryParamDecoderMatcher[String]("triggerMode")
  object NodeParam extends OptionalQueryParamDecoderMatcher[String]("node")
  object ShapeParam extends OptionalQueryParamDecoderMatcher[String]("shape")
  object NameParam extends OptionalQueryParamDecoderMatcher[String]("name")
  object ShapeMapParameter extends OptionalQueryParamDecoderMatcher[String]("shapeMap")
  object ShapeMapURLParameter extends OptionalQueryParamDecoderMatcher[String]("shapeMapURL")
  object ShapeMapFileParameter extends OptionalQueryParamDecoderMatcher[String]("shapeMapFile")
  object ShapeMapFormatParam extends OptionalQueryParamDecoderMatcher[String]("shapeMapFormat")
  object SchemaEmbedded extends OptionalQueryParamDecoderMatcher[Boolean]("schemaEmbedded")
  object InferenceParam extends OptionalQueryParamDecoderMatcher[String]("inference")
  object ExamplesParam extends OptionalQueryParamDecoderMatcher[String]("examples")
  object ManifestURLParam extends OptionalQueryParamDecoderMatcher[String]("manifestURL")
  object OptExamplesParam extends OptionalQueryParamDecoderMatcher[String]("examples")
  object OptQueryParam extends OptionalQueryParamDecoderMatcher[String]("query")
  object OptActiveDataTabParam extends OptionalQueryParamDecoderMatcher[String]("activeDataTab")
  object OptActiveSchemaTabParam extends OptionalQueryParamDecoderMatcher[String]("activeSchemaTab")
  object OptActiveShapeMapTabParam extends OptionalQueryParamDecoderMatcher[String]("activeShapeMapTab")
  object OptActiveQueryTabParam extends OptionalQueryParamDecoderMatcher[String]("activeQueryTab")

}