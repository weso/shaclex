import sbt._
import sbt.Keys._
import sbtunidoc.Plugin.UnidocKeys._
import com.typesafe.sbt.SbtGit.GitKeys._

name := "shaclex"

lazy val shaclexVersion = "0.0.55"

cancelable in Global := true
fork := true
reStartArgs := Seq("--server")

parallelExecution in Test := false

// Versions of common packages
lazy val circeVersion     = "0.6.0"
lazy val effCatsVersion   = "2.0.0-RC18"
lazy val catsVersion      = "0.8.1"
lazy val scalaTestVersion = "3.0.0"
lazy val scalacticVersion = "3.0.0"
lazy val logbackVersion   = "1.1.7"
lazy val loggingVersion   = "3.5.0"
lazy val http4sVersion    = "0.14.11a"
lazy val rhoVersion       = "0.12.0a"

herokuAppName in Compile := "shaclex"

herokuJdkVersion in Compile := "1.8"

herokuProcessTypes in Compile := Map(
  "web" -> "target/universal/stage/bin/shaclex --server -Dhttp.port=$PORT"
)

lazy val commonSettings = Seq(
  organization := "es.weso",
  scalaVersion := "2.11.8",
  version := shaclexVersion,
  scalaOrganization := "org.typelevel",
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
  libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % scalacticVersion
    , "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )
)

lazy val shaclex =
  project.in(file(".")).
  settings(unidocSettings: _*).
  settings(commonSettings:_*).
  settings(publishSettings:_*).
  enablePlugins(BuildInfoPlugin).
  aggregate(schema,shacl,shex,manifest,srdfJena,srdf,utils,converter,rbe,typing,validating,server).
  dependsOn(schema,shacl,shex,manifest,srdfJena,srdf,utils,converter,rbe,typing,validating,server).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "es.weso.shaclex.buildinfo",
    unidocProjectFilter in (ScalaUnidoc, unidoc) :=
      inAnyProject -- inProjects(noDocProjects: _*),
    libraryDependencies ++=
      Seq(
        "ch.qos.logback" %  "logback-classic" % logbackVersion
      , "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
      , "org.rogach" %% "scallop" % "2.0.2"
      )
  )


lazy val schema =
  project.in(file("modules/schema")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  dependsOn(shex, shacl)


lazy val shacl =
  project.in(file("modules/shacl")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  dependsOn(srdfJena,
            manifest,
            utils,
            typing,
            validating).
  settings(
   libraryDependencies ++=
     Seq(
       "com.typesafe" % "config" % "1.3.0" % Test
     , "org.typelevel" %% "cats" % catsVersion
     )
  )

lazy val compatTest = config("compat") extend (Test) describedAs("Tests that check compatibility (some may fail)")

lazy val shex =
  project.in(file("modules/shex")).
  configs(compatTest).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  dependsOn(srdfJena,
    srdf,
    typing,
    utils % "test -> test; compile -> compile",
    validating,
    rbe).
  settings(antlr4Settings: _*).
  settings(inConfig(compatTest)(Defaults.testSettings): _*).
  settings(
    antlr4GenListener in Antlr4 := true,
    antlr4GenVisitor in Antlr4 := true,
    antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5",
    antlr4PackageName in Antlr4 := Some("es.weso.shex.parser"),
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.0" % Test
    , "ch.qos.logback" %  "logback-classic" % logbackVersion
    , "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
    , "io.circe" %% "circe-core" % circeVersion
    , "io.circe" %% "circe-generic" % circeVersion
    , "io.circe" %% "circe-parser" % circeVersion
    )
  )

lazy val server =
  project.in(file("modules/server")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  enablePlugins(BuildInfoPlugin).
  dependsOn(schema,srdf,srdfJena).
  enablePlugins(SbtTwirl).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "es.weso.shaclex.buildinfo",
    libraryDependencies ++= Seq(
      "org.http4s" %% "rho-swagger" % rhoVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-twirl" % http4sVersion
    )
  )

lazy val converter =
  project.in(file("modules/converter")).
  settings(publishSettings: _*).
  settings(commonSettings: _*).
  dependsOn(shex,shacl)

lazy val manifest =
  project.in(file("modules/manifest")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  dependsOn(srdfJena, utils).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.0" % Test
    , "com.github.nikita-volkov" % "sext" % "0.2.4"
    )
  )

lazy val srdf =
  project.in(file("modules/srdf")).
  settings(commonSettings: _*).
  settings(publishSettings: _*)

lazy val rbe =
  project.in(file("modules/rbe")).
  dependsOn(validating).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  settings(
   libraryDependencies ++= 
     Seq(
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
   , "org.typelevel" %% "cats" % catsVersion
	 )
  )
  

lazy val srdfJena =
  project.in(file("modules/srdfJena")).
  dependsOn(srdf).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic" % logbackVersion
    , "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
    , "com.typesafe" % "config" % "1.3.0" % Test
    , "org.apache.jena" % "jena-arq" % "3.1.0"
    )
  )

lazy val typing =
  project.in(file("modules/typing")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % catsVersion
    )
  )

lazy val utils =
  project.in(file("modules/utils")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.atnos" %% "eff-cats" % effCatsVersion
    , "io.circe" %% "circe-core" % circeVersion
    , "io.circe" %% "circe-generic" % circeVersion
    , "io.circe" %% "circe-parser" % circeVersion
    , "org.typelevel" %% "cats" % catsVersion
    )
  )

lazy val validating =
  project.in(file("modules/validating")).
  settings(commonSettings: _*).
  settings(publishSettings: _*).
  dependsOn(srdfJena,
            utils % "test -> test; compile -> compile").
  settings(antlr4Settings: _*).
  settings(
   addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0"),
   addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0"),
   libraryDependencies ++= Seq(
     "org.atnos" %% "eff-cats" % effCatsVersion
   , "org.typelevel" %% "cats" % catsVersion
   )
  )



def noDocProjects: Seq[ProjectReference] = Seq[ProjectReference](
 //  benchmark,
 validating
)

lazy val docSettings = unidocSettings ++ Seq(
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-groups",
    "-implicits",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo := "git@github.com:labra/shaclex.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.svg" | "*.js" | "*.swf" | "*.yml" | "*.md"
)


lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)


// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")

// Binary packaging
enablePlugins(SbtNativePackager)
enablePlugins(WindowsPlugin)
enablePlugins(JavaAppPackaging)

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
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"


resolvers += Resolver.bintrayRepo("labra", "maven")

// EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

// Publishing settings to BinTray

publishMavenStyle := true

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

// licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

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
//  "-Xfatal-warnings",
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


