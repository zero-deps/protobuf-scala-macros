resolvers += Resolver.jcenterRepo

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.6")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.25")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.9.3"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "latest.integration"
libraryDependencies += "io.github.zero-deps" %% "gs-git" % "1.6.1"
