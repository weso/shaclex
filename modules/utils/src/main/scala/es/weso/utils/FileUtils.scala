package es.weso.utils
import java.io._

import scala.io._
import util._

import scala.util.{Failure, Success, Try}

object FileUtils {

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

  def getFileFromFolderWithExt(path: String,
                               name: String,
                               ext: String): File = {
    new File(path + "/" + name + "." + ext)
  }

  def splitExtension(str: String): (String, String) = {
    val splits = str.split('.')
    (splits.init.mkString("."), splits.last)
  }

 /**
   * Ensures to close a file.
   * Follows the [[https://wiki.scala-lang.org/display/SYGN/Loan Loan pattern]]
   */
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

  /**
    * Gets the contents of a file
    *
    * @param file file
    *
    */
  def getContents(file: File): Try[CharSequence] = {
    try {
      using(Source.fromFile(file)("UTF-8")) { source =>
        Success(source.getLines.mkString("\n"))
      }
    } catch {
      case e: FileNotFoundException => {
        Failure(e)
      }
      case e: IOException => {
        Failure(e)
      }
    }
  }

  /**
   * Gets the contents of a file
   *
   * @param fileName name of the file
   *
   */
  def getContents(fileName: String): Try[CharSequence] = {
    try {
      using(Source.fromFile(fileName)("UTF-8")) { source =>
        Success(source.getLines.mkString("\n"))
      }
    } catch {
      case e: FileNotFoundException => {
        Failure(e)
      }
      case e: IOException => {
        Failure(e)
      }
    }
  }

  def getStream(fileName: String): Try[InputStreamReader] = {
    try {
      using(Source.fromFile(fileName)("UTF-8")) { source => {
        Success(source.reader())
       }
      }
    } catch {
      case e: FileNotFoundException => {
        Failure(e)
      }
      case e: IOException => {
        Failure(e)
      }
    }
  }
    /**
   * Write contents to a file
   *
   * @param name name of the file
   * @param contents contents to write to the file
   */
  def writeFile(name: String, contents: String): Unit = {
    import java.nio.file._
    val path = Paths.get(name)
    Files.write(path, contents.getBytes)
    ()
  }

}
