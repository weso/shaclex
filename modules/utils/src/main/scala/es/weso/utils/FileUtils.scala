package es.weso.utils
import java.io._
import java.nio.file.Paths

import scala.io._
import util._

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

  def getFileFromFolderWithSameExt(
                                 file: File,
                                 oldExt: String,
                                 newExt: String): Either[String,File] = {
    val newName = file.getAbsolutePath.reverse.replaceFirst(oldExt.reverse, newExt.reverse).reverse
    Try {
      new File(newName)
    }.fold(exc =>
      Left(s"Error accessing file with name $newName: ${exc.getMessage}"),
      Right(_))
  }


  def getFileFromFolderWithExt(
    path: String,
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
  def getContents(file: File): Either[String, CharSequence] = {
    try {
      using(Source.fromFile(file)("UTF-8")) { source =>
        Right(source.getLines.mkString("\n"))
      }
    } catch {
      case e: FileNotFoundException =>
        Left(s"Error reading file ${file.getAbsolutePath}: ${e.getMessage}")
      case e: IOException =>
        Left(s"IO Exception reading file ${file.getAbsolutePath}: ${e.getMessage}")
      case e: Exception =>
        Left(s"Exception reading file ${file.getAbsolutePath}: ${e.getMessage}")
    }
  }

  /**
   * Gets the contents of a file
   *
   * @param fileName name of the file
   *
   */
  def getContents(fileName: String): Either[String, CharSequence] = {
    try {
      using(Source.fromFile(fileName)("UTF-8")) { source =>
        Right(source.getLines.mkString("\n"))
      }
    } catch {
      case e: FileNotFoundException =>
        Left(s"Error reading file ${fileName}: ${e.getMessage}")
      case e: IOException =>
        Left(s"IO Exception reading file ${fileName}: ${e.getMessage}")
      case e: Exception =>
        Left(s"Exception reading file ${fileName}: ${e.getMessage}")
    }
  }

  def getStream(fileName: String): Either[String, InputStreamReader] = {
    try {
      using(Source.fromFile(fileName)("UTF-8")) { source =>
        {
          Right(source.reader())
        }
      }
    } catch {
      case e: FileNotFoundException =>
        Left(s"Error reading file ${fileName}: ${e.getMessage}")
      case e: IOException =>
        Left(s"IO Exception reading file ${fileName}: ${e.getMessage}")
      case e: Exception =>
        Left(s"Exception reading file ${fileName}: ${e.getMessage}")
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

  /**
   * Format a char sequence including the line numbers
   * @param cs
   * @return String with the line numbers of the char sequence
   */
  def formatLines(cs: CharSequence): String = {
    cs.toString.linesIterator.zipWithIndex.map(p => (p._2 + 1).toString + " " + p._1).mkString("\n")
  }

  lazy val currentFolderURL: String =
    Paths.get(".").normalize.toUri.toURL.toExternalForm
//    ""

}
