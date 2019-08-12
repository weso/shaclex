package es.weso.utils

sealed abstract class ParserReport[+A, +B]

final case class Parsed[A](info: A)
  extends ParserReport[A, Nothing]

final case class NotParsed[B](error: B)
  extends ParserReport[Nothing, B]
