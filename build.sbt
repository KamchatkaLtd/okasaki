name := "prio_queues"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.3.4" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.3.4" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
