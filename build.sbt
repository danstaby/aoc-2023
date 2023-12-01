import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.danstaby"
ThisBuild / organizationName := "aoc"

lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.15"

lazy val root = (project in file("."))
  .settings(
    name := "aoc",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += munit % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
