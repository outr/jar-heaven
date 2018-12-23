name := "jar-heaven"
organization := "com.outr"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "io.get-coursier" %% "coursier" % "1.1.0-M9",
  "io.get-coursier" %% "coursier-cache" % "1.1.0-M9"
)
