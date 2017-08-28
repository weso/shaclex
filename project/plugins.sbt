addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
// addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.1.0")
addSbtPlugin("com.eed3si9n"             % "sbt-buildinfo"          % "0.7.0")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

// addSbtPlugin("com.jsuereth"         	  % "sbt-pgp"                % "1.0.0")

addSbtPlugin("com.typesafe.sbt" 		    % "sbt-site" 			         % "1.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")
addSbtPlugin("com.heroku" % "sbt-heroku" % "2.0.0")
addSbtPlugin("pl.project13.scala"       % "sbt-jmh"                % "0.2.27")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.1")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
// addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.1.1")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.19")
addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.3.4")
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.0")
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.1")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

resolvers += Classpaths.sbtPluginReleases

resolvers +=  Resolver.url(
  "scala-js-releases", url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns)

resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)


resolvers += Resolver.sonatypeRepo("releases")		