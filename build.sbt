ThisBuild / organization := "io.github.zero-deps"
ThisBuild / version := zd.gs.git.GitOps.version
ThisBuild / scalaVersion := "3.0.0-M1"
val scala213 = "2.13.4"
ThisBuild / scalacOptions ++= Seq(
  "-Ywarn-extra-implicit",
//   "-Xfatal-warnings",
//   "-deprecation",
//   "-feature",
//   "-unchecked",
//   "-Ywarn-unused:implicits",
//   "-Ywarn-unused:imports",
//   "-Yno-completion",
//   "-Ywarn-numeric-widen",
//   "-Ywarn-value-discard",
)
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / isSnapshot := true

ThisBuild / turbo := true
ThisBuild / useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file("."))
  .dependsOn(scala3macros).aggregate(scala3macros, runtime)
  .settings(
    name := "proto",
    scalaVersion := scala213,
    scalacOptions += "-Ytasty-reader",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0-RC3" % Test,
    skip in publish := true,
  )

lazy val scala2macros = project.in(file("macros"))
  .dependsOn(runtime)
  .settings(
    name := "proto-macros-scala2",
    scalaVersion := scala213,
    scalacOptions += "-Ytasty-reader",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scala213
  )

lazy val scala3macros = project.in(file("scala3macros"))
  .dependsOn(runtime, scala2macros)
  .settings(
    name := "proto-macros",
  )

lazy val scala3test = project.in(file("scala3test"))
  .dependsOn(scala3macros)
  .settings(
    name := "proto-macros-test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    skip in publish := true,
  )

lazy val runtime = project.in(file("runtime")).settings(
  name := "proto-runtime",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.10.0",
)

lazy val benchmark = project.in(file("benchmark")).settings(
  scalaVersion := scala213,
  scalacOptions += "-Ytasty-reader",
  libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.0",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.0.1",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.0.1" % Provided,
  libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.1",
  resolvers += Resolver.bintrayRepo("evolutiongaming", "maven"),
  libraryDependencies += "com.evolutiongaming" %% "kryo-macros" % "1.3.0",
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  ),
  skip in publish := true,
).enablePlugins(JmhPlugin).dependsOn(scala3macros)
