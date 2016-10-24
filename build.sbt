name := "ordered"

version := "0.1"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq("-feature", "-language:higherKinds")
