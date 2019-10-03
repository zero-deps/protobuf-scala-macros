ThisBuild / organization := "io.github.zero-deps"
ThisBuild / version := zd.gs.git.GitOps.version(tags=true)
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / scalacOptions ++= Seq(
  "-Ywarn-extra-implicit",
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Yno-completion",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / isSnapshot := true

ThisBuild / turbo := true
ThisBuild / useCoursier := true
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project.in(file(".")).settings(
  name := "proto",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0-SNAP13" % Test,
  skip in publish := true,
).dependsOn(macros).aggregate(macros, runtime, purs, benchmark)

lazy val macros = project.in(file("macros")).settings(
  name := "proto-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime)

lazy val runtime = project.in(file("runtime")).settings(
  name := "proto-runtime",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.7.1",
)

lazy val purs = project.in(file("purs")).settings(
  name := "proto-purs",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0-SNAP13" % Test,
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
).dependsOn(runtime, macros % Test)

lazy val benchmark = project.in(file("benchmark")).settings(
  libraryDependencies += "org.scodec" %% "scodec-core" % "1.11.4",
  libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.0",
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "0.55.4" % Compile,
  libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.55.4" % Provided,
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  ),
  skip in publish := true,
).enablePlugins(JmhPlugin).dependsOn(macros)
