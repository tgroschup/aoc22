ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "Advent of Code 22"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"