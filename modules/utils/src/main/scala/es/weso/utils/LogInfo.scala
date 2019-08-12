package es.weso.utils

object LogInfo {
  var level: Int = 0
  val indent: Int = 2

  def apply(msg: String, incrementLevel: Int = 0): Unit = {
    level += incrementLevel
    val sb = new StringBuilder
    (1 to level * indent).foreach { _ => sb.append(" ") }
    println(sb.toString ++ s"$msg")
  }

}

