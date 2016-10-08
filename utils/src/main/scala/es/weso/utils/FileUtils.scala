package es.weso.utils
import java.io.File
import scala.io._

object MyFileUtils {

  def getFilesFromFolderWithExt(
    path: String,
    ext: String,
    ignoreFiles: List[String]): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter { file =>
        if (file.isFile) {
          val (name, extension) = splitExtension(file.getName)
          extension == ext && !ignoreFiles.contains(name)
        } else false
      }.toList
    } else {
      List[File]()
    }
  }

  def splitExtension(str: String): (String, String) = {
    val splits = str.split('.')
    (splits.init.mkString("."), splits.last)
  }

}
