
import sbt._
import Keys._

object Build extends Build {
  lazy val commonSettings = Seq(
    version := "1.0",
    scalaVersion := "2.10.4"
  )

  val dependencies = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test",
    "com.typesafe" % "config" % "1.2.1"
  )

  lazy val core = project
    .in(file("core"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= dependencies
    )

  lazy val tests = project
    .in(file("tests"))
    .dependsOn(core)

  lazy val root = project
    .in(file("."))
    .aggregate(core, tests)
}