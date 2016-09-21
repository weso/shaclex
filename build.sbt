import sbt._
import sbt.Keys._

lazy val shaclex = 
  project.in(file(".")).
  settings(publishSettings:_*)

name := "shaclex"

organization := "es.weso"

version := "0.0.4"

scalaVersion := "2.11.8"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

val circeVersion = "0.5.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
    "org.rogach" %% "scallop" % "2.0.1" 
  , "com.typesafe" % "config" % "1.3.0"
  , "org.slf4j" % "slf4j-simple" % "1.7.21"
  , "log4j" % "log4j" % "1.2.17"
  , "org.slf4s" % "slf4s-api_2.11" % "1.7.13"
  , "org.scalatest" %% "scalatest" % "3.0.0" 
  , "org.typelevel" %% "cats" % "0.7.2"
  , "com.lihaoyi" %% "pprint" % "0.4.1"
  , "org.atnos" %% "eff-cats" % "2.0.0-RC2-20160814085121-d925e69"
//  , "es.weso" % "shexcala_2.11" % "0.7.16"
  , "es.weso" % "srdf-jena_2.11" % "0.0.8" 
  , "es.weso" % "validating_2.11" % "0.0.16" 
  , "es.weso" % "weso_utils_2.11" % "0.0.15" 
  , "org.specs2" %% "specs2-core" % "3.8.4" % "test"
  )

antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5"

antlr4PackageName in Antlr4 := Some("es.weso.shex.parser")
  
autoCompilerPlugins := true

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.0" cross CrossVersion.binary)

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

// addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0" cross CrossVersion.full)

// Needed by simulacrum
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// Binary packaging
enablePlugins(SbtNativePackager)
enablePlugins(JavaAppPackaging)
enablePlugins(WindowsPlugin)

// general package information 
maintainer := "Jose Emilio Labra Gayo <labra@uniovi.es>"
packageSummary in Linux := "shaclex"
packageSummary in Windows := "shaclex"
packageDescription := "shaclex"

// wix build information
wixProductId := "39b564d5-d381-4282-ada9-87244c76e14b"
wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
// The same numbers as in the docs?
// wixProductId := "ce07be71-510d-414a-92d4-dff47631848a"
// wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"

mainClass in Compile := Some("es.weso.shaclex.Main")

mainClass in assembly := Some("es.weso.shaclex.Main")

test in assembly := {}

assemblyJarName in assembly := "shaclex.jar"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.bintrayRepo("labra", "maven")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// Publishing settings to BinTray

publishMavenStyle := true

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

resolvers += Resolver.bintrayRepo("labra", "maven")

ghpages.settings

git.remoteRepo := "git@github.com:labra/shaclex.git"

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/labra/shaclex")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/labra/shaclex"), "scm:git:git@github.com:labra/shaclex.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://labra.github.io/shaclex/latest/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>labra</id>
        <name>Jose Emilio Labra Gayo</name>
        <url>https://github.com/labra/</url>
      </developer>
    </developers>
  ),
  scalacOptions in (Compile,doc) ++= Seq(
    "-diagrams-debug",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams",
    "-Yrangepos",
    "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
  )
)


