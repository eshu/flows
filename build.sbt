
scalaVersion := "2.13.16"

lazy val root = (project in file(".")).settings(
  name         := "flows",
  organization := "io.h8",
  version      := "0.0.1",
  scalacOptions ++= Seq(
    "-Xsource:3",
    "--explain-types",
    "--language:higherKinds",
    "--deprecation",
    "--feature",
    "--unchecked",
    //"-Wunused:_",
    "-Wdead-code",
    "-Xlint:_",
    "-Xfatal-warnings"
  ),
  libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "2.13.0")
)
