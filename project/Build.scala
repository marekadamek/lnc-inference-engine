
import sbt._
import Keys._

object Build extends Build {
  lazy val commonSettings = Seq(
    version := "1.0",
    scalaVersion := "2.10.4"
  )

  val dependencies = Seq(
    "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"
  )

  val javaapiDependencies = Seq(
    "junit" % "junit" % "4.12",
    "com.novocode" % "junit-interface" % "0.10" % "test"
  )

  lazy val core = project
    .in(file("core"))
    .settings(commonSettings: _*)
    .settings(
      libraryDependencies ++= dependencies
    )

  lazy val javaapi = project
    .in(file("javaapi"))
    .dependsOn(core)
    .settings(
      libraryDependencies ++= javaapiDependencies
    )

  lazy val tests = project
    .in(file("tests"))
    .dependsOn(core)

  lazy val root = project
    .in(file("."))
    .aggregate(core, javaapi, tests)
}