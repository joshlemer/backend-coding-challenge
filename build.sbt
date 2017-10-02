name := "backend-coding-challenge"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies := Seq(
  "com.github.pathikrit" %% "better-files"  % "3.1.0",
  "ch.hsr" % "geohash" % "1.3.0",
  "com.rockymadden.stringmetric" % "stringmetric-core_2.11" % "0.27.4",
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.10"
)
    