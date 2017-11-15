package es.weso.server

import org.http4s.dsl.io.{OptionalQueryParamDecoderMatcher, QueryParamDecoderMatcher}

object QueryParams {
  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object OptDataParam extends OptionalQueryParamDecoderMatcher[String]("data")
  object OptDataURLParam extends OptionalQueryParamDecoderMatcher[String]("dataURL")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object TargetDataFormatParam extends OptionalQueryParamDecoderMatcher[String]("targetDataFormat")
  object SchemaParam extends OptionalQueryParamDecoderMatcher[String]("schema")
  object SchemaURLParam extends OptionalQueryParamDecoderMatcher[String]("schemaURL")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")
  object TargetSchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("targetSchemaFormat")
  object TargetSchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("targetSchemaEngine")
  object TriggerModeParam extends OptionalQueryParamDecoderMatcher[String]("triggerMode")
  object NodeParam extends OptionalQueryParamDecoderMatcher[String]("node")
  object ShapeParam extends OptionalQueryParamDecoderMatcher[String]("shape")
  object NameParam extends OptionalQueryParamDecoderMatcher[String]("name")
  object ShapeMapParam extends OptionalQueryParamDecoderMatcher[String]("shapeMap")
  object SchemaEmbedded extends OptionalQueryParamDecoderMatcher[Boolean]("schemaEmbedded")
  object InferenceParam extends OptionalQueryParamDecoderMatcher[String]("inference")
  object ExamplesParam extends QueryParamDecoderMatcher[String]("examples")
  object OptExamplesParam extends OptionalQueryParamDecoderMatcher[String]("examples")
  object OptQueryParam extends OptionalQueryParamDecoderMatcher[String]("query")
  object OptActiveDataTabParam extends OptionalQueryParamDecoderMatcher[String]("activeDataTab")

}