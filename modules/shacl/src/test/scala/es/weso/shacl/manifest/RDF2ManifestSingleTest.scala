package es.weso.shacl.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._
//import sext._

import scala.util._

class RDF2ManifestSingleTest extends FunSpec
  with Matchers with TryValues with OptionValues {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclStdTests")
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
    parseManifest("and-001","core/node/")
  }

  def parseManifest(name: String,folder: String): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      val fileName = s"$shaclFolder/$folder/$name.ttl"
      RDF2Manifest.read(fileName, "TURTLE", Some(s"$shaclFolderURI$folder/"), true) match {
        case Left(e) =>
          fail(s"Error reading $fileName\n$e")
        case Right(pair) =>
          val (mf,_) = pair
          info(s"Manifest successfully read. ${mf.label.getOrElse("")}")
      }
    }
  }
}
