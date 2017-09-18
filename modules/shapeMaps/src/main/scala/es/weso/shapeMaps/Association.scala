package es.weso.shapeMaps

import io.circe._
import io.circe.syntax._

case class Association(
  nodeSelector: NodeSelector,
  shapeLabel: ShapeMapLabel,
  info: Info = Info()) {

  def toJson: Json = {
    this.asJson
  }

}

object Association {
  import NodeSelector._
  import ShapeMapLabel._

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
