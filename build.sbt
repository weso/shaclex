import sbt._
import sbt.Keys._

lazy val shaclex =
  project.in(file(".")).
  settings(publishSettings:_*).
  settings(commonSettings:_*).
  aggregate(shacl,shex,manifest,srdfJena,srdf,utils).
  dependsOn(shacl,shex,manifest,srdfJena,srdf,utils).
  settings(
    libraryDependencies ++=
      Seq(
       "org.rogach" %% "scallop" % "2.0.1"
      )
  )

lazy val shacl =
  project.in(file("shacl")).
  settings(commonSettings: _*).
  dependsOn(srdfJena,
            manifest,
            utils,
            validating).
  settings(
   libraryDependencies ++=
     Seq(
       "org.slf4s" % "slf4s-api_2.11" % "1.7.13"
     , "com.typesafe" % "config" % "1.3.0" % Test
     , "org.typelevel" %% "cats" % catsVersion
     )
  )

lazy val shex =
  project.in(file("shex")).
  settings(commonSettings: _*).
  dependsOn(srdfJena,
            utils % "test -> test; compile -> compile",
            validating).
  settings(antlr4Settings: _*).
  settings(
    antlr4GenListener in Antlr4 := true,
    antlr4GenVisitor in Antlr4 := true,
    antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5",
    antlr4PackageName in Antlr4 := Some("es.weso.shex.parser"),
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.0" % Test
    , "ch.qos.logback" %  "logback-classic" % "1.1.7"
    , "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
    , "io.circe" %% "circe-core" % circeVersion
    , "io.circe" %% "circe-generic" % circeVersion
    , "io.circe" %% "circe-parser" % circeVersion
    )
  )

lazy val validating =
  project.in(file("validating")).
  settings(commonSettings: _*).
  dependsOn(srdfJena,utils % "test -> test; compile -> compile").
  settings(antlr4Settings: _*).
  settings(
   addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0"),
   addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),
   libraryDependencies ++= Seq(
     "org.atnos" %% "eff-cats" % effCatsVersion
   , "org.typelevel" %% "cats" % catsVersion
   )
  )


lazy val manifest =
  project.in(file("manifest")).
  settings(commonSettings: _*).
  dependsOn(srdfJena, utils).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.0" % Test
    , "com.github.nikita-volkov" % "sext" % "0.2.4"
    )
  )

lazy val srdf =
  project.in(file("srdf")).
  settings(commonSettings: _*)

lazy val srdfJena =
  project.in(file("srdfJena")).
  dependsOn(srdf).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.0" % Test
    , "org.apache.jena" % "jena-arq" % "3.1.0"
    )
  )

lazy val converter =
  project.in(file("converter")).
  settings(commonSettings: _*).
  dependsOn(shex,shacl)


lazy val utils =
  project.in(file("utils")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.atnos" %% "eff-cats" % effCatsVersion
    , "io.circe" %% "circe-core" % circeVersion
    , "io.circe" %% "circe-generic" % circeVersion
    , "io.circe" %% "circe-parser" % circeVersion
    , "org.typelevel" %% "cats" % catsVersion
    )
  )

lazy val commonSettings = Seq(
  organization := "es.weso",
  version := "0.0.4",
  scalaVersion := "2.11.8",
  scalaOrganization := "org.typelevel",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
  antlr4PackageName in Antlr4 := Some("es.weso.shex.parser"),
  libraryDependencies ++= Seq(
//    "com.typesafe" %%% "config" % "1.3.0" % Test
    "org.scalactic" %% "scalactic" % "3.0.0"
  , "org.scalatest" %% "scalatest" % "3.0.0" % Test
//  , "com.lihaoyi" %% "pprint" % "0.4.1"
//  , "es.weso" % "srdf-jvm_2.11" % "0.0.9"
//  , "es.weso" % "weso_utils_2.11" % "0.0.15"
//  , "com.github.nikita-volkov" %%% "sext" % "0.2.4"
  )
)

name := "shaclex"

// Versions of common packages
lazy val circeVersion = "0.5.1"
lazy val effCatsVersion = "2.0.0-RC11"
lazy val catsVersion = "0.7.2"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
//addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

// addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0" cross CrossVersion.full)
// addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")

// Needed by simulacrum
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// Binary packaging
enablePlugins(SbtNativePackager)
enablePlugins(JavaAppPackaging)
enablePlugins(WindowsPlugin)

// general package information
maintainer := "Jose Emilio Labra Gayo <labra@uniovi.es>"
packageSummary in Linux := name.value
packageSummary in Windows := name.value
packageDescription := name.value

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
  "-Xfuture",
  "-Ypartial-unification", // enable fix for SI-2712
  "-Yliteral-types"       // enable SIP-23 implementation
  )
)


