addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.19")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.8.2"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "latest.integration"
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "latest.integration"