lazy val scala212 = "2.12.15"
lazy val scala213 = "2.13.8"
lazy val scala3   = "3.1.1"

lazy val supportedScalaVersions = List(
  scala3,
  scala212,
  scala213
)

val Java11 = JavaSpec.temurin("11") // "adopt@1.11"

// Local dependencies
lazy val srdfVersion  = "0.1.107"
lazy val utilsVersion = "0.2.4"
lazy val shexVersion  = "0.2.2"
lazy val shaclVersion = "0.1.83"

// Dependency versions
lazy val catsVersion           = "2.7.0"
lazy val circeVersion          = "0.14.1"
lazy val jenaVersion           = "4.4.0"
lazy val jgraphtVersion        = "1.3.1"
lazy val jlineVersion          = "3.21.0"
lazy val jnaVersion            = "5.11.0"
lazy val logbackVersion        = "1.2.11"
lazy val loggingVersion        = "3.9.4"
lazy val munitVersion          = "0.7.29"
lazy val munitEffectVersion    = "1.0.7"
lazy val pprintVersion         = "0.7.3"
lazy val rdf4jVersion          = "3.7.6"
lazy val scalacheckVersion     = "1.15.4"
lazy val scallopVersion        = "4.1.0"
lazy val shaclTQVersion        = "1.4.0"
lazy val typesafeConfigVersion = "1.4.2"

// Compiler plugin dependency versions
lazy val simulacrumVersion = "1.0.0"
// lazy val kindProjectorVersion = "0.9.5"
// lazy val scalaMacrosVersion   = "2.1.1"

// WESO components
lazy val srdf       = "es.weso" %% "srdf"       % srdfVersion
lazy val srdfJena   = "es.weso" %% "srdfjena"   % srdfVersion
lazy val srdf4j     = "es.weso" %% "srdf4j"     % srdfVersion
lazy val utils      = "es.weso" %% "utils"      % utilsVersion
lazy val typing     = "es.weso" %% "typing"     % utilsVersion
lazy val validating = "es.weso" %% "validating" % utilsVersion
lazy val utilsTest  = "es.weso" %% "utilstest"  % utilsVersion
lazy val shex       = "es.weso" %% "shex"       % shexVersion
lazy val shexTest   = "es.weso" %% "shextest"   % shexVersion
lazy val shapemap   = "es.weso" %% "shapemap"   % shexVersion
lazy val shacl      = "es.weso" %% "shacl"      % shaclVersion

// Other dependency modules
lazy val catsCore       = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsKernel     = "org.typelevel" %% "cats-kernel" % catsVersion
lazy val circeCore      = "io.circe" %% "circe-core" % circeVersion
lazy val circeGeneric   = "io.circe" %% "circe-generic" % circeVersion
lazy val circeParser    = "io.circe" %% "circe-parser" % circeVersion
lazy val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
lazy val jenaArq        = "org.apache.jena" % "jena-arq" % jenaVersion
lazy val jenaShacl      = "org.apache.jena" % "jena-shacl" % jenaVersion
lazy val jenaShEx       = "org.apache.jena" % "jena-shex" % jenaVersion
lazy val jenaFuseki     = "org.apache.jena" % "jena-fuseki-main" % jenaVersion
lazy val jline          = "org.jline" % "jline" % jlineVersion
lazy val jna            = "net.java.dev.jna" % "jna" % jnaVersion
lazy val munit          = "org.scalameta" %% "munit" % munitVersion
lazy val munitEffect    = "org.typelevel" %% "munit-cats-effect-3" % munitEffectVersion
lazy val MUnitFramework = new TestFramework("munit.Framework")

lazy val pprint         = "com.lihaoyi"                %% "pprint"        % pprintVersion
lazy val rdf4j_runtime  = "org.eclipse.rdf4j"          % "rdf4j-runtime"  % rdf4jVersion
lazy val shaclTQ        = "org.topbraid"               % "shacl"          % shaclTQVersion
lazy val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
lazy val scallop        = "org.rogach"                 %% "scallop"       % scallopVersion
lazy val typesafeConfig = "com.typesafe"               % "config"         % typesafeConfigVersion

ThisBuild / githubWorkflowJavaVersions := Seq(Java11)

lazy val shaclex = project
  .in(file("."))
  .enablePlugins(
    ScalaUnidocPlugin,
    SiteScaladocPlugin,
    AsciidoctorPlugin,
//    SbtNativePackager,
    WindowsPlugin,
    JavaAppPackaging,
    LauncherJarPlugin
  )
  //  .disablePlugins(RevolverPlugin)
  //  .settings(
  //    buildInfoKeys := BuildInfoKey.ofN(name, version, scalaVersion, sbtVersion),
  //    buildInfoPackage := "es.weso.shaclex.buildinfo"
  //  )
  .settings(commonSettings, packagingSettings, ghPagesSettings, publishSettings, wixSettings)
  .aggregate(schemaInfer, schema, converter, slang, sgraph)
  .dependsOn(schemaInfer, schema, converter, slang, sgraph)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    // ScalaUnidoc / siteSubdirName := "scaladoc/latest",
    // addMappingsToSiteDir(mappings in(ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(noDocProjects: _*),
    makeSite / mappings ++= Seq(
      file("src/assets/favicon.ico") -> "favicon.ico"
    ),
    libraryDependencies ++= Seq(
      logbackClassic,
      scalaLogging,
      scallop,
      typesafeConfig,
      shexTest,
      jline,
      jna
    ),
    Global / cancelable := true,
    fork := true,
    //    parallelExecution in Test := false,
    ThisBuild / turbo := true
    //   skip in publish := true
  )

lazy val schemaInfer = project
  .in(file("modules/schemaInfer"))
  //  .disablePlugins(RevolverPlugin)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(srdf)
  )
  .dependsOn(
    schema
  )

lazy val schema = project
  .in(file("modules/schema"))
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(
      scalaLogging,
      srdf,
      srdfJena,
      srdf4j,
      shex,
      shacl,
      shapemap,
      jenaShacl,
      jenaShEx,
      shaclTQ,
      munitEffect % Test
    ),
    testFrameworks += MUnitFramework
  )
  .dependsOn(
    slang,
    converter
  )

lazy val slang = project
  .in(file("modules/slang"))
  //  .disablePlugins(RevolverPlugin)
  .settings(commonSettings, publishSettings)
  .dependsOn(
    )
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      scalaLogging,
      catsCore,
      catsKernel,
      // catsMacros,
      shex,
      shacl,
      utils,
      srdf,
      srdf4j   % Test,
      srdfJena % Test
    )
  )

lazy val sgraph = project
  .in(file("modules/sgraph"))
  //  .disablePlugins(RevolverPlugin)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(
      scalaLogging,
      utils,
      utilsTest % Test,
      srdf,
      catsCore,
      catsKernel,
      // catsMacros,
      srdf4j      % Test,
      srdfJena    % Test,
      munit       % Test,
      munitEffect % Test
    ),
    testFrameworks += MUnitFramework
  )

lazy val converter = project
  .in(file("modules/converter"))
  //  .disablePlugins(RevolverPlugin)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(
      scalaLogging,
      logbackClassic,
      scalaLogging,
      srdfJena % Test,
      shex,
      shacl,
      pprint,
      munit       % Test,
      munitEffect % Test
    ),
    testFrameworks += MUnitFramework
  )

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noDocProjects = Seq[ProjectReference]()

lazy val sharedDependencies = Seq(
  libraryDependencies ++= Seq(
    // scalactic,
    // scalaTest % Test
  )
)

lazy val packagingSettings = Seq(
  // Do not package logback files in .jar, they interfere with other logback
  // files in classpath
  Compile / packageBin / mappings ~= { project =>
    project.filter {
      case (file, _) =>
        val fileName = file.getName
        !(fileName.startsWith("logback") && (fileName.endsWith(".xml") || fileName
          .endsWith(".groovy")))
    }
  },
  Compile / mainClass := Some("es.weso.shaclex.Main"),
  assembly / mainClass := Some("es.weso.shaclex.Main"),
  assembly / test := {},
  assembly / assemblyJarName := "shaclex.jar",
  Linux / packageSummary := name.value,
  Windows / packageSummary := name.value,
  packageDescription := name.value
)

lazy val compilationSettings = Seq(
  // scalaVersion := "2.13.1",
  // format: off
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.  "-encoding", "UTF-8",
    "-language:_",
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xlint",
    "-Yrangepos",
    "-Ywarn-dead-code", // Warn when dead code is identified.
    // "-Xfatal-warnings",
    "-Ywarn-extra-implicit"  // Warn when more than one implicit parameter section is defined.
  )
  // format: on
)

lazy val wixSettings = Seq(
  wixProductId := "39b564d5-d381-4282-ada9-87244c76e14b",
  wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
  // The same numbers as in the docs?
  // wixProductId := "ce07be71-510d-414a-92d4-dff47631848a",
  // wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"
)

lazy val ghPagesSettings = Seq(
  //  git.remoteRepo := "git@github.com:weso/shaclex.git"
)

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  organization := "es.weso",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots")
  )
)

//noinspection HttpUrlsUsage
lazy val publishSettings = Seq(
  //  maintainer      := "Jose Emilio Labra Gayo <labra@uniovi.es>",
  homepage := Some(url("https://github.com/weso/shaclex")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/weso/shaclex"), "scm:git:git@github.com:labra/shaclex.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://weso.github.io/shaclex/latest/api/")),
  /* pomExtra        := <developers>
                        <developer>
                          <id>labra</id>
                          <name>Jose Emilio Labra Gayo</name>
                          <url>https://labra.weso.es/</url>
                        </developer>
                      </developers>, */
  publishMavenStyle := true,
  sonatypeProfileName := "es.weso",
  homepage := Some(url("https://github.com/weso/shaclex")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/weso/shaclex"), "scm:git:git@github.com:weso/shaclex.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://weso.github.io/shaclex/latest/api/")),
  autoAPIMappings := true,
  developers := List(
    Developer(
      id = "labra",
      name = "Jose Emilio Labra Gayo",
      email = "jelabra@gmail.com",
      url = url("https://labra.weso.es")
    )
  )
)
