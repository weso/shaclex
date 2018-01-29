package es.weso.server

import cats._
import cats.effect.IO
import data._
import implicits._
import org.http4s.multipart.Part
import fs2.text.utf8Decode

case class PartsMap private(map: Map[String,Part[IO]]) {

  def optPartValue(key: String): IO[Option[String]] =
    map.get(key) match {
      case Some(part) =>
        part.body.through(utf8Decode).compile.foldMonoid.map(Some.apply)
      case None => IO(None)
    }

  def optPartValueBoolean(key: String): IO[Option[Boolean]] = map.get(key) match {
    case Some(part) => part.body.through(utf8Decode).compile.foldMonoid.map {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    }
    case None => IO(None)
  }

}

object PartsMap {

  def apply(ps: Vector[Part[IO]]): PartsMap = {
    PartsMap(ps.filter(_.name.isDefined).map(p => (p.name.get,p)).toMap)
  }


}