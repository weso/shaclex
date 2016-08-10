addSbtPlugin("com.github.gseitz"    	% "sbt-release"         % "1.0.0")
addSbtPlugin("com.jsuereth"         	% "sbt-pgp"             % "1.0.0")
addSbtPlugin("com.typesafe.sbteclipse" 	% "sbteclipse-plugin" 	% "4.0.0")
addSbtPlugin("com.typesafe.sbt" 		% "sbt-site" 			% "1.0.0")
addSbtPlugin("com.eed3si9n" 			% "sbt-assembly" 		% "0.14.3")
addSbtPlugin("com.typesafe.sbt" 		% "sbt-ghpages" 		% "0.5.4")
//addSbtPlugin("org.scala-js" 			% "sbt-scalajs" 		% "0.6.5")
addSbtPlugin("com.typesafe.sbt" 		% "sbt-native-packager" % "1.0.3")
addSbtPlugin("com.typesafe.sbt" 		% "sbt-scalariform" 	% "1.3.0")
addSbtPlugin("me.lessis" 				% "bintray-sbt" 		% "0.3.0")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

resolvers += Classpaths.sbtPluginReleases

resolvers +=  Resolver.url(
  "scala-js-releases", url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns)
  
resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

