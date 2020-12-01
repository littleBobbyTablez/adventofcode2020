name := "AdventOfCode2020"

version := "0.1"

scalaVersion := "2.12.12"


libraryDependencies ++= Seq("org.typelevel" %% "cats-effect" % "2.0.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.tomakehurst" % "wiremock" % "2.26.2" % Test)