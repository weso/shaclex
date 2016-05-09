import sbt._
import sbt.Keys._

lazy val root = project.in(file("."))

organization := "es.weso"

name := "shaclex"

scalaVersion := "2.11.8"

version := "0.0.1"

libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-simple" % "1.6.4"
  , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
  , "commons-configuration" % "commons-configuration" % "1.7"
  , "org.rogach" %% "scallop" % "0.9.5" 
  , "com.typesafe" % "config" % "1.0.1"
  , "org.scala-lang" % "scala-compiler" % scalaVersion.value
  , "org.scalatest" %%% "scalatest" % "3.0.0-M15" 
  , "org.typelevel" %% "cats" % "0.6.0-M1"
//  , "org.scoverage" %% "scalac-scoverage-runtime" % "1.0.4"  
//  , "com.storm-enroute" %% "scalameter" % "0.8-SNAPSHOT"
  , "es.weso" % "shexcala_2.11" % "0.7.9" excludeAll(ExclusionRule(organization = "org.slf4j"))  
  , "es.weso" % "validating_2.11" % "0.0.2"   
  )

autoCompilerPlugins := true


// Binari packaging
enablePlugins(JavaAppPackaging)

packageSummary in Linux := "shaclex"
packageSummary in Windows := "shaclex"
packageDescription := "shaclex"

maintainer in Windows := "WESO"
maintainer in Debian := "Jose Emilio Labra <jelabra@gmail.com>"

mainClass in Compile := Some("es.weso.shaclex.Main")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

//resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

resolvers += Resolver.bintrayRepo("labra", "maven")


EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

