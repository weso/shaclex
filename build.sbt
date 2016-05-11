import sbt._
import sbt.Keys._

lazy val root = project.in(file(".")).
  settings(publishSettings:_*).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
)

name := "shaclex"

organization := "es.weso"

version := "0.0.1"

scalaVersion := "2.11.8"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value  
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value
  , "org.scalatest" %%% "scalatest" % "3.0.0-M15" 
  , "org.typelevel" %% "cats" % "0.6.0-M1"
  , "es.weso" % "shexcala_2.11" % "0.7.11" excludeAll(ExclusionRule(organization = "org.slf4j"))  
  , "es.weso" % "validating_2.11" % "0.0.6"   
  )

autoCompilerPlugins := true


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

site.settings

site.includeScaladoc()

mainClass in Compile := Some("es.weso.shaclex.Main")

mainClass in assembly := Some("es.weso.shaclex.Main")

test in assembly := {}

assemblyJarName in assembly := "shaclex.jar"

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

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
//    "-Xfatal-warnings",
    "-diagrams-debug",
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  )
)


