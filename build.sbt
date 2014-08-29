name := "sorted-streams"

version := "0.1"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.scalaz" %% "scalaz-core" % "7.0.6"
)

scalacOptions ++= Seq("-feature", "-language:higherKinds")
