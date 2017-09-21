package es.weso.shapeMaps

import cats.Show
import cats.syntax.show._
import io.circe._
import io.circe.syntax._
import NodeSelector._
import ShapeMapLabel._

case class Association(
  nodeSelector: NodeSelector,
  shapeLabel: ShapeMapLabel,
  info: Info = Info()) {

  def toJson: Json = {
    this.asJson
  }

}

object Association {

  implicit val encodeAssociation: Encoder[Association] = new Encoder[Association] {
    final def apply(a: Association): Json = {
      Json.fromJsonObject(JsonObject.empty.
        add("nodeSelector", a.nodeSelector.asJson).
        add("shapeLabel", a.shapeLabel.asJson).
        add("status", a.info.status.asJson).
        add("reason", a.info.reason.fold(Json.Null)(Json.fromString(_))).
        add("appInfo", a.info.appInfo))
    }
  }
}
