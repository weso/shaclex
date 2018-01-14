/*
scalafmt: {
  style = defaultWithAlign
  maxColumn = 150
  align.tokens = [
    { code = "=>", owner = "Case" }
    { code = "⇒", owner = "Case" }
    { code = "extends", owner = "Defn.(Class|Trait|Object)" }
    { code = "//", owner = ".*" }
    { code = "{", owner = "Template" }
    { code = "}", owner = "Template" }
    { code = ":=", owner = "Term.ApplyInfix" }
    { code = "++=", owner = "Term.ApplyInfix" }
    { code = "+=", owner = "Term.ApplyInfix" }
    { code = "%", owner = "Term.ApplyInfix" }
    { code = "%%", owner = "Term.ApplyInfix" }
    { code = "%%%", owner = "Term.ApplyInfix" }
    { code = "->", owner = "Term.ApplyInfix" }
    { code = "→", owner = "Term.ApplyInfix" }
    { code = "<-", owner = "Enumerator.Generator" }
    { code = "←", owner = "Enumerator.Generator" }
    { code = "=", owner = "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))" }
  ]
}
 */
name := "shaclex"

lazy val scalaCompilerVersion = "2.12.4"
lazy val projectVersion       = "0.0.1"

cancelable in Global := true
fork                 := true
reStartArgs          := Seq("--server")

parallelExecution in Test := false

// Dependency versions
lazy val antlrVersion          = "4.6"
lazy val circeVersion          = "0.9.0-M2"
lazy val effVersion            = "4.5.0"
lazy val catsVersion           = "1.0.0-RC1"
lazy val scalaTestVersion      = "3.0.4"
lazy val scalacticVersion      = "3.0.4"
lazy val logbackVersion        = "1.2.3"
lazy val loggingVersion        = "3.7.2"
lazy val http4sVersion         = "0.18.0-M5"
lazy val scalatagsVersion      = "0.6.2"
lazy val scallopVersion        = "2.0.6"
lazy val jenaVersion           = "3.4.0"
lazy val jgraphtVersion        = "1.0.1"
lazy val diffsonVersion        = "2.2.2"
lazy val xercesVersion         = "2.11.0"
lazy val sextVersion           = "0.2.4"
lazy val scalaGraphVersion     = "1.11.5"
lazy val typesafeConfigVersion = "1.3.0"
lazy val scalacheckVersion     = "1.13.4"

// Compiler plugin dependency versions
lazy val simulacrumVersion    = "0.11.0"
lazy val kindProjectorVersion = "0.9.3"
lazy val scalaMacrosVersion   = "2.1.0"

// Dependency modules
lazy val logbackClassic    = "ch.qos.logback"             % "logback-classic"      % logbackVersion
lazy val typesafeConfig    = "com.typesafe"               % "config"               % typesafeConfigVersion
lazy val sext              = "com.github.nikita-volkov"   % "sext"                 % sextVersion
lazy val jgraphtCore       = "org.jgrapht"                % "jgrapht-core"         % jgraphtVersion
lazy val antlr4            = "org.antlr"                  % "antlr4"               % antlrVersion
lazy val xercesImpl        = "xerces"                     % "xercesImpl"           % xercesVersion
lazy val jenaArq           = "org.apache.jena"            % "jena-arq"             % jenaVersion
lazy val scalaLogging      = "com.typesafe.scala-logging" %% "scala-logging"       % loggingVersion
lazy val scallop           = "org.rogach"                 %% "scallop"             % scallopVersion
lazy val scalactic         = "org.scalactic"              %% "scalactic"           % scalacticVersion
lazy val scalaTest         = "org.scalatest"              %% "scalatest"           % scalaTestVersion
lazy val catsCore          = "org.typelevel"              %% "cats-core"           % catsVersion
lazy val catsKernel        = "org.typelevel"              %% "cats-kernel"         % catsVersion
lazy val catsMacros        = "org.typelevel"              %% "cats-macros"         % catsVersion
lazy val circeCore         = "io.circe"                   %% "circe-core"          % circeVersion
lazy val circeGeneric      = "io.circe"                   %% "circe-generic"       % circeVersion
lazy val circeParser       = "io.circe"                   %% "circe-parser"        % circeVersion
lazy val http4sDsl         = "org.http4s"                 %% "http4s-dsl"          % http4sVersion
lazy val http4sBlazeServer = "org.http4s"                 %% "http4s-blaze-server" % http4sVersion
lazy val http4sBlazeClient = "org.http4s"                 %% "http4s-blaze-client" % http4sVersion
lazy val http4sCirce       = "org.http4s"                 %% "http4s-circe"        % http4sVersion
lazy val http4sTwirl       = "org.http4s"                 %% "http4s-twirl"        % http4sVersion
lazy val scalatags         = "com.lihaoyi"                %% "scalatags"           % scalatagsVersion
lazy val eff               = "org.atnos"                  %% "eff"                 % effVersion
lazy val scalacheck        = "org.scalacheck"             %% "scalacheck"          % scalacheckVersion
lazy val diffsonCirce      = "org.gnieh"                  %% "diffson-circe"       % diffsonVersion

// Compiler plugin modules
lazy val scalaMacrosParadise = "org.scalamacros"      % "paradise"        % scalaMacrosVersion cross CrossVersion.full
lazy val simulacrum          = "com.github.mpilquist" %% "simulacrum"     % simulacrumVersion
lazy val kindProjector       = "org.spire-math"       %% "kind-projector" % kindProjectorVersion

lazy val shaclex =
  project
    .in(file("."))
    .enablePlugins(ScalaUnidocPlugin)
    .enablePlugins(ScalaUnidocPlugin)
    .settings(commonSettings, publishSettings)
    .aggregate(schema, shacl, shex, manifest, srdfJena, srdf, utils, converter, rbe, typing, validating, server, shapeMaps, depGraphs)
    .dependsOn(schema, shacl, shex, manifest, srdfJena, srdf, utils, converter, rbe, typing, validating, server, shapeMaps, depGraphs)
    .settings(
      unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(noDocProjects: _*),
      libraryDependencies ++=
        Seq(
          logbackClassic,
          scalaLogging,
          scallop
        )
    )

lazy val commonSettings = Seq(
  organization := "es.weso",
  scalaVersion := scalaCompilerVersion,
  libraryDependencies ++= Seq(
    scalactic,
    scalaTest % Test
  ),
  scalacOptions ++= Seq(
    "-Yrangepos",
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.  "-encoding", "UTF-8",
    "-language:_",
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    // "-Xfatal-warnings",    // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",  // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
//  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
//  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
//  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
//  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
//  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
//  "-Ywarn-unused:privates",            // Warn if a private member is unused.
//  "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    "-Ypartial-unification" // enable fix for SI-2712
  )
)

lazy val schema =
  project
    .in(file("modules/schema"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(shex, shacl, shapeMaps)

lazy val depGraphs =
  project
    .in(file("modules/depGraphs"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .settings(
      libraryDependencies ++=
        Seq(
          catsCore,
          catsKernel,
          catsMacros,
          jgraphtCore
        )
    )

lazy val shacl =
  project
    .in(file("modules/shacl"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(srdfJena, manifest, utils, typing, validating)
    .settings(
      logBuffered in Test       := false,
      parallelExecution in Test := false,
      fork in Test              := true,
      libraryDependencies ++=
        Seq(
          typesafeConfig % Test,
          sext,
          catsCore,
          catsKernel,
          catsMacros
        )
    )

lazy val shex =
  project
    .in(file("modules/shex"))
    .
//  configs(compatTest).
    settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(srdfJena, srdf, typing, utils % "test -> test; compile -> compile", validating, shapeMaps, rbe, manifest, depGraphs)
    .
//  settings(antlr4Settings: _*).
    enablePlugins(Antlr4Plugin)
    .
//  settings(inConfig(compatTest)(Defaults.testSettings): _*).
    settings(
    antlr4GenListener in Antlr4 := true,
    antlr4GenVisitor in Antlr4  := true,
    antlr4Dependency in Antlr4  := antlr4,
    antlr4PackageName in Antlr4 := Some("es.weso.shex.parser"),
    libraryDependencies ++= Seq(
      typesafeConfig % Test,
      logbackClassic,
      scalaLogging,
      circeCore,
      circeGeneric,
      circeParser
    )
  )

lazy val shapeMaps =
  project
    .in(file("modules/shapeMaps"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(srdfJena)
    .enablePlugins(Antlr4Plugin)
    .
//    settings(antlr4Settings: _*).
    settings(
    antlr4GenListener in Antlr4 := true,
    antlr4GenVisitor in Antlr4  := true,
    antlr4Dependency in Antlr4  := antlr4,
    antlr4PackageName in Antlr4 := Some("es.weso.shapeMaps.parser"),
    libraryDependencies ++=
      Seq(
        sext,
        scalaLogging,
        catsCore,
        catsKernel,
        catsMacros,
        circeCore,
        circeGeneric,
        circeParser
      )
  )

lazy val server =
  project
    .in(file("modules/server"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .
    // enablePlugins(BuildInfoPlugin).
    dependsOn(schema, srdf, srdfJena)
    .enablePlugins(SbtTwirl)
    .settings(
      //  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      //  buildInfoPackage := "es.weso.shaclex.buildinfo",
      libraryDependencies ++= Seq(
//      "org.http4s" %% "rho-swagger" % rhoVersion,
        http4sDsl,
        http4sBlazeServer,
        http4sBlazeClient,
        http4sCirce,
        http4sTwirl,
        scalatags
      ),
      resolvers += Resolver.sonatypeRepo("snapshots")
    )

lazy val converter =
  project
    .in(file("modules/converter"))
    .settings(publishSettings: _*)
    .settings(commonSettings: _*)
    .dependsOn(shex, shacl)

lazy val manifest =
  project
    .in(file("modules/manifest"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(srdfJena, utils)
    .settings(
      libraryDependencies ++= Seq(
        typesafeConfig % Test,
        sext
      )
    )

lazy val rbe =
  project
    .in(file("modules/rbe"))
    .dependsOn(validating, typing)
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .settings(
      libraryDependencies ++=
        Seq(
          compilerPlugin(scalaMacrosParadise),
          simulacrum,
          catsCore,
          catsKernel,
          catsMacros,
          scalacheck % Test
        )
    )

lazy val srdf =
  project
    .in(file("modules/srdf"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        catsCore,
        catsKernel,
        catsMacros,
        circeCore,
        circeGeneric,
        circeParser,
        scalaLogging
      )
    )

lazy val srdfJena =
  project
    .in(file("modules/srdfJena"))
    .dependsOn(srdf, utils)
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        logbackClassic,
        scalaLogging,
        typesafeConfig % Test,
        jenaArq,
        catsCore,
        catsKernel,
        catsMacros
      )
    )

lazy val typing =
  project
    .in(file("modules/typing"))
    .settings(commonSettings: _*)
    .
//  settings(publishSettings: _*).
    settings(
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsMacros
    )
  )

lazy val utils =
  project
    .in(file("modules/utils"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .settings(
      libraryDependencies ++= Seq(
        eff,
        circeCore,
        circeGeneric,
        circeParser,
        catsCore,
        catsKernel,
        catsMacros,
        diffsonCirce,
        xercesImpl
      )
    )

lazy val validating =
  project
    .in(file("modules/validating"))
    .settings(commonSettings: _*)
    .settings(publishSettings: _*)
    .dependsOn(srdfJena, utils % "test -> test; compile -> compile")
    .
//  settings(antlr4Settings: _*).
    settings(
    addCompilerPlugin(kindProjector),
    libraryDependencies ++= Seq(
      eff,
      catsCore,
      catsKernel,
      catsMacros
//   , "org.typelevel" %% "cats-mtl-core" % "0.0.2"
// , "org.typelevel" %% "cats-mtl" % catsVersion
    )
  )

def noDocProjects: Seq[ProjectReference] = Seq[ProjectReference](
  //  benchmark,
  validating
)

lazy val docSettings = Seq(
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-groups",
    "-implicits",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath
  ),
  git.remoteRepo            := "git@github.com:labra/shaclex.git",
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.svg" | "*.js" | "*.swf" | "*.yml" | "*.md"
)

lazy val noPublishSettings = Seq(
//  publish := (),
//  publishLocal := (),
  publishArtifact := false
)

// to write types like Reader[String, ?]
addCompilerPlugin(kindProjector)

// Binary packaging
enablePlugins(SbtNativePackager)
enablePlugins(WindowsPlugin)
enablePlugins(JavaAppPackaging)

// general package information
maintainer                := "Jose Emilio Labra Gayo <labra@uniovi.es>"
packageSummary in Linux   := name.value
packageSummary in Windows := name.value
packageDescription        := name.value

// wix build information
wixProductId        := "39b564d5-d381-4282-ada9-87244c76e14b"
wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
// The same numbers as in the docs?
// wixProductId := "ce07be71-510d-414a-92d4-dff47631848a"
// wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"

mainClass in Compile := Some("es.weso.shaclex.Main")

mainClass in assembly := Some("es.weso.shaclex.Main")

test in assembly := {}

assemblyJarName in assembly := "shaclex.jar"

// resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
// resolvers += Resolver.sonatypeRepo("releases")

// resolvers += Resolver.sonatypeRepo("snapshots")

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Sonatype OSS Snapshots" at
//  "https://oss.sonatype.org/content/repositories/snapshots"

// resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

// EclipseKeys.eclipseOutput := Some(".target")
// EclipseKeys.withJavadoc := true
// EclipseKeys.useProjectId := true
// EclipseKeys.skipParents in ThisBuild := false

// Publishing settings to BinTray

publishMavenStyle := true

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

resolvers += Resolver.bintrayRepo("labra", "maven")
resolvers += Resolver.bintrayRepo("weso", "weso-releases")

// ghpages.settings

git.remoteRepo := "git@github.com:labra/shaclex.git"

lazy val publishSettings = Seq(
  homepage        := Some(url("https://github.com/labra/shaclex")),
  licenses        := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo         := Some(ScmInfo(url("https://github.com/labra/shaclex"), "scm:git:git@github.com:labra/shaclex.git")),
  autoAPIMappings := true,
  apiURL          := Some(url("http://labra.github.io/shaclex/latest/api/")),
  pomExtra        := <developers>
                       <developer>
                         <id>labra</id>
                         <name>Jose Emilio Labra Gayo</name>
                         <url>https://github.com/labra/</url>
                       </developer>
                     </developers>,
  scalacOptions in doc ++= Seq(
    "-diagrams-debug",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams",
  )
)
