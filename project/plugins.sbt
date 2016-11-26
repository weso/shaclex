addSbtPlugin("com.eed3si9n"             % "sbt-unidoc"             % "0.3.3")
addSbtPlugin("com.github.gseitz"    	  % "sbt-release"            % "1.0.0")
addSbtPlugin("com.jsuereth"         	  % "sbt-pgp"                % "1.0.0")
addSbtPlugin("com.typesafe.sbt" 		    % "sbt-site" 			         % "1.0.0")
addSbtPlugin("com.eed3si9n" 			      % "sbt-assembly" 		       % "0.14.3")
addSbtPlugin("com.typesafe.sbt" 		    % "sbt-ghpages" 		       % "0.5.4")
addSbtPlugin("com.heroku"               % "sbt-heroku"             % "1.0.1")
addSbtPlugin("pl.project13.scala"       % "sbt-jmh"                % "0.2.15")
addSbtPlugin("com.typesafe.sbt" 		    % "sbt-native-packager"    % "1.1.4")
addSbtPlugin("me.lessis" 				        % "bintray-sbt" 		       % "0.3.0")
addSbtPlugin("org.scalastyle"           %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("org.scoverage"            % "sbt-scoverage"          % "1.5.0-RC1")
addSbtPlugin("org.wartremover"          % "sbt-wartremover"        % "1.1.1")
addSbtPlugin("io.spray"                 % "sbt-revolver"           % "0.8.0")
addSbtPlugin("org.scala-js"             % "sbt-scalajs"            % "0.6.12")
addSbtPlugin("com.typesafe.sbt"         % "sbt-twirl"              % "1.2.0")
addSbtPlugin("com.typesafe.sbt" 		    % "sbt-scalariform" 	     % "1.3.0")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

resolvers += Classpaths.sbtPluginReleases

resolvers +=  Resolver.url(
  "scala-js-releases", url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(Resolver.ivyStylePatterns)

resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

