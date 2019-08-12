package es.weso.slang

case class SchemaS(lblMap: Map[Label, SLang]) {
  def getLabel(lbl: Label): Option[SLang] = lblMap.get(lbl)

  def availableLabels: Set[Label] = lblMap.keySet
}
