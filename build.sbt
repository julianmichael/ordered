name := "sorted-streams"

version := "0.1"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  // "com.chuusai" %% "shapeless" % "2.3.2",
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq("-feature", "-language:higherKinds")
