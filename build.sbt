lazy val root = project.in(file("."))
  .aggregate(orderedJVM, orderedJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val ordered = crossProject.settings(
  name := "ordered",
  organization := "org.me",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
  ),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalacOptions ++= Seq("-feature", "-language:higherKinds")
)

lazy val orderedJVM = ordered.jvm
lazy val orderedJS = ordered.js

