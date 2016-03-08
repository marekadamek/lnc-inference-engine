import sbt._

object Build extends Build {
  lazy val root = project
    .in(file("."))
    .aggregate(core, javaapi, app)

  lazy val core = project

  lazy val javaapi = project
    .dependsOn(core)

  lazy val app = project
    .dependsOn(core)
}